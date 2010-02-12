-- | Atom C code generation.
module AtomAVR.H4x.Language.Atom.Code
  ( Config (..)
  , writeC
  , showC
  , defaults
  , cTypes
  , c99Types
  , RuleCoverage
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import System.IO
import Text.Printf

import Language.Atom.Analysis
import Language.Atom.Elaboration
import Language.Atom.Expressions
import Language.Atom.Scheduling

-- | C code configuration parameters.
data Config = Config
  { cFuncName     :: String                                                  -- ^ Alternative primary function name.  Leave empty to use compile name.
  , cType         :: Type -> String                                          -- ^ C type naming rules.
  , cCode         :: [Name] -> [Name] -> [(Name, Type)] -> (String, String)  -- ^ Custom C code to insert above and below, given assertion names, coverage names, and probe names and types.
  , cRuleCoverage :: Bool                                                    -- ^ Enable rule coverage tracking.
  , cAssert       :: Bool                                                    -- ^ Enable assertions and functional coverage.
  , cAssertName   :: String                                                  -- ^ Name of assertion function.  Type: void assert(int, cType Bool, cType Word64);
  , cCoverName    :: String                                                  -- ^ Name of coverage function.  Type: void cover(int, cType Bool, cType Word64);
  }

-- | Default C code configuration parameters (default function name, no pre/post code, ANSI C types).
defaults :: Config
defaults = Config
  { cFuncName     = ""
  , cType         = cTypes
  , cCode         = \ _ _ _ -> ("", "")
  , cRuleCoverage = True
  , cAssert       = True
  , cAssertName   = "assert"
  , cCoverName    = "cover"
  }

showConst :: Const -> String
showConst c = case c of
  CBool   c -> if c then "1" else "0"
  CInt8   c -> show c
  CInt16  c -> show c
  CInt32  c -> show c ++ "L"
  CInt64  c -> show c ++ "LL"
  CWord8  c -> show c
  CWord16 c -> show c
  CWord32 c -> show c ++ "UL"
  CWord64 c -> show c ++ "ULL"
  CFloat  c -> show c
  CDouble c -> show c

-- | ANSI C type naming rules.
cTypes :: Type -> String
cTypes t = case t of
  Bool   -> "unsigned char"
  Int8   -> "signed char"
  Int16  -> "signed short"
  Int32  -> "signed long"
  Int64  -> "signed long long"
  Word8  -> "unsigned char"
  Word16 -> "unsigned short"
  Word32 -> "unsigned long"
  Word64 -> "unsigned long long"
  Float  -> "float"
  Double -> "double"

-- | C99 type naming rules.
c99Types :: Type -> String
c99Types t = case t of
  Bool   -> "uint8_t"
  Int8   -> "int8_t"
  Int16  -> "int16_t"
  Int32  -> "int32_t"
  Int64  -> "int64_t"
  Word8  -> "uint8_t"
  Word16 -> "uint16_t"
  Word32 -> "uint32_t"
  Word64 -> "uint64_t"
  Float  -> "float"
  Double -> "double"

codeUE :: Config -> [(UE, String)] -> String -> (UE, String) -> String
codeUE config ues d (ue, n) = d ++ cType config (typeOf ue) ++ " " ++ n ++ " = " ++ basic operands ++ ";\n"
  where
  operands = map (fromJust . flip lookup ues) $ ueUpstream ue
  basic :: [String] -> String
  basic operands = concat $ case ue of
    UVRef (UV i n _)                 -> ["__v", show i, " /* ", n, " */ "]
    UVRef (UVArray (UA i n _) _)     -> ["__a", show i, "[", a, "] /* ", n, " */ "]
    UVRef (UVArray (UAExtern n _) _) -> [n, "[", a, "]"]
    UVRef (UVExtern n _)             -> [n]
    UCast _ _            -> ["(", cType config (typeOf ue), ") ", a]
    UConst c             -> [showConst c]
    UAdd _ _             -> [a, " + ", b]
    USub _ _             -> [a, " - ", b]
    UMul _ _             -> [a, " * ", b]
    UDiv _ _             -> [a, " / ", b]
    UMod _ _             -> [a, " % ", b]
    UNot _               -> ["! ", a]
    UAnd _               -> intersperse " && " operands
    UBWNot _             -> ["~ ", a]
    UBWAnd _ _           -> [a, " & ", b]
    UBWOr  _ _           -> [a, " | ", b]
    UShift _ n           -> (if n >= 0 then [a, " << ", show n] else [a, " >> ", show (negate n)])
    UEq  _ _             -> [a, " == ", b]
    ULt  _ _             -> [a, " < " , b]
    UMux _ _ _           -> [a, " ? " , b, " : ", c]
    UF2B _               -> ["*((", ct Word32, " *) &(", a, "))"]
    UD2B _               -> ["*((", ct Word64, " *) &(", a, "))"]
    UB2F _               -> ["*((", ct Float , " *) &(", a, "))"]
    UB2D _               -> ["*((", ct Double, " *) &(", a, "))"]
    where
    ct = cType config
    a = head operands
    b = operands !! 1
    c = operands !! 2

type RuleCoverage = [(Name, Int, Int)]


writeC :: Name -> Config -> [Rule] -> Schedule -> [UV] -> [UA] -> [Name] -> [Name] -> [(Name, Type)] -> IO RuleCoverage
writeC name config rules schedule uvs uas assertionNames coverageNames probeNames = do
  writeFile (name ++ ".c") c
  return cov
  where
  (c, cov) = showC name config rules schedule uvs uas assertionNames coverageNames probeNames

showC :: Name -> Config -> [Rule] -> Schedule -> [UV] -> [UA] -> [Name] -> [Name] -> [(Name, Type)] -> (String, RuleCoverage)
showC name config rules schedule uvs uas assertionNames coverageNames probeNames = (c, cov)
  where
  cov = [ (ruleName r, div (ruleId r) 32, mod (ruleId r) 32) | r <- rules' ]
  (preCode, postCode) = cCode config assertionNames coverageNames probeNames
  c = unlines
    [ preCode
    , ""
    , "static " ++ cType config Word64 ++ " __global_clock = 0;"
    , codeIf (cRuleCoverage config) $ "static const " ++ cType config Word32 ++ " __coverage_len = " ++ show covLen ++ ";"
    , codeIf (cRuleCoverage config) $ "static " ++ cType config Word32 ++ " __coverage[" ++ show covLen ++ "] = {" ++ (concat $ intersperse ", " $ replicate covLen "0") ++ "};"
    , codeIf (cRuleCoverage config) $ "static " ++ cType config Word32 ++ " __coverage_index = 0;"
    , concatMap (declUV config) uvs
    , concatMap (declUA config) uas
    , concatMap (codeRule config) rules'
    , codeAssertionChecks config assertionNames coverageNames rules
    , "void " ++ (if null (cFuncName config) then name else cFuncName config) ++ "(void) {"
    , concatMap (codePeriodPhase config) schedule
    , "  __global_clock = __global_clock + 1;"
    , "}"
    , ""
    , postCode
    ]

  rules' :: [Rule]
  rules' = concat [ r | (_, _, r) <- schedule ]

  covLen = 1 + div (maximum $ map ruleId rules') 32

codeIf :: Bool -> String -> String
codeIf a b = if a then b else ""

declUA :: Config -> UA -> String
declUA _ (UAExtern _ _) = error "declUA"
declUA config ua@(UA i n init) = concat ["static ", cType config (typeOf ua), " __a", show i, "[", show (length init), "] = {", intercalate "," (map formatConst init), "};  /* ", n, " */\n"]

declUV :: Config -> UV -> String
declUV _ (UVArray _ _)  = error "declUV"
declUV _ (UVExtern _ _) = error "declUV"
declUV config (UV i n init) = concat ["static ", cType config (typeOf init), " __v", show i, " = ", formatConst init, ";  /* ", n, " */\n"]

formatConst :: Const -> String
formatConst c = case c of
  CBool True  -> "1"
  CBool False -> "0"
  CInt8   a   -> show a
  CInt16  a   -> show a
  CInt32  a   -> show a ++ "L"
  CInt64  a   -> show a ++ "LL"
  CWord8  a   -> show a
  CWord16 a   -> show a
  CWord32 a   -> show a ++ "UL"
  CWord64 a   -> show a ++ "ULL"
  CFloat  a   -> show a ++ "F"
  CDouble a   -> show a

codeRule :: Config -> Rule -> String
codeRule config rule@(Rule _ _ _ _ _ _ _) =
  "/* " ++ show rule ++ " */\n" ++
  "static void __r" ++ show (ruleId rule) ++ "(void) {\n" ++
  concatMap (codeUE    config ues "  ") ues ++
  "  if (" ++ id (ruleEnable rule) ++ ") {\n" ++
  concatMap codeAction (ruleActions rule) ++
  codeIf (cRuleCoverage config) ("    __coverage[" ++ covWord ++ "] = __coverage[" ++ covWord ++ "] | (1 << " ++ covBit ++ ");\n") ++
  "  }\n" ++
  concatMap codeAssign (ruleAssigns rule) ++
  "}\n\n"
  where
  ues = topo $ allUEs rule
  id ue = fromJust $ lookup ue ues

  codeAction :: (([String] -> String), [UE]) -> String
  codeAction (f, args) = "    " ++ f (map id args) ++ ";\n"

  covWord = show $ div (ruleId rule) 32
  covBit  = show $ mod (ruleId rule) 32

  codeAssign :: (UV, UE) -> String
  codeAssign (uv, ue) = concat ["  ", lh, " = ", id ue, ";\n"]
    where
    lh = case uv of
      UV i n _                     -> concat ["__v", show i, " /* ", n, " */"]
      UVArray (UA i n _)     index -> concat ["__a", show i, "[", id index, "] /* ", n, " */"]
      UVArray (UAExtern n _) index -> concat [n, "[", id index, "]"]
      UVExtern n _                 -> n

codeRule _ _ = ""

codeAssertionChecks :: Config -> [Name] -> [Name] -> [Rule] -> String
codeAssertionChecks config assertionNames coverageNames rules = codeIf (cAssert config) $
  "static void __assertion_checks(void) {\n" ++
  concatMap (codeUE config ues "  ") ues ++
  concat [ "  if (" ++ id enable ++ ") " ++ cAssertName config ++ "(" ++ assertionId name ++ ", " ++ id check ++ ", __global_clock);\n" | Assert name enable check <- rules ] ++
  concat [ "  if (" ++ id enable ++ ") " ++ cCoverName  config ++ "(" ++ coverageId  name ++ ", " ++ id check ++ ", __global_clock);\n" | Cover  name enable check <- rules ] ++
  "}\n\n"
  where
  ues = topo $ concat [ [a, b] | Assert _ a b <- rules ] ++ concat [ [a, b] | Cover _ a b <- rules ]
  id ue = fromJust $ lookup ue ues
  assertionId :: Name -> String
  assertionId name = show $ fromJust $ elemIndex name assertionNames
  coverageId :: Name -> String
  coverageId name = show $ fromJust $ elemIndex name coverageNames

codePeriodPhase :: Config -> (Int, Int, [Rule]) -> String
codePeriodPhase config (period, phase, rules) = unlines
  [ printf "  {"
  , printf "    static %s __scheduling_clock = %i;" (cType config clockType) phase
  , printf "    if (__scheduling_clock == 0) {"
  , intercalate "\n" $ map callRule rules
  , printf "      __scheduling_clock = %i;" (period - 1)
  , printf "    }"
  , printf "    else {"
  , printf "      __scheduling_clock = __scheduling_clock - 1;"
  , printf "    }"
  , printf "  }"
  ]
  where
  clockType | period < 2 ^  8 = Word8
            | period < 2 ^ 16 = Word16
            | otherwise       = Word32
  callRule r = concat ["      ", codeIf (cAssert config) "__assertion_checks(); ", "__r", show (ruleId r), "();  /* ", show r, " */"]

