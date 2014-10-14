import Prelude
import System.Environment
import Text.Printf
infixl 4 </>
infixl 4 <|>
infixl 4 <@>
infixl 4 <->

(</>) :: String -> String -> String
d </> f = d ++ "/" ++ f

(<|>) :: String -> String -> String
s1 <|> s2 = s1 ++ "," ++ s2

(<@>) :: String -> String -> String
s1 <@> s2 = s1 ++ "\n" ++ s2

(<->) :: String -> String -> String
s1 <-> s2 = s1 ++ ",," ++ s2

main :: IO()
main = do
 [d] <- getArgs
 f   <- readFile (d </> "result")
 let ls   = (read f):: [(String,Float)]
     lk s = maybe "No Data" (printf "%.2f") (lookup s ls)
 writeFile (init d ++ ".csv") $
   "(QMiniFeldspar)"<|>"Haskell Compile"                  <|> "Haskell Run"                 <|>"C Compile"                    <|>"C Run"                        <->
   "(MiniFeldspar)" <|>"Haskell Compile"                  <|> "Haskell Run"                 <|>"C Compile"                    <|>"C Run"                        <@>
   "IPGray"         <|> lk "IPGrayTemplateHaskellCompile" <|> lk "IPGrayTemplateHaskellGen" <|> lk "IPGrayTemplateHaskellGCC" <|> lk "IPGrayTemplateHaskellRun" <->
   "IPGray"         <|> lk "IPGrayMiniWellScopedCompile"    <|> lk "IPGrayMiniWellScopedGen"    <|> lk "IPGrayMiniWellScopedGCC"    <|> lk "IPGrayMiniWellScopedRun"    <@>
   "IPBW"           <|> lk "IPBWTemplateHaskellCompile"   <|> lk "IPBWTemplateHaskellGen"   <|> lk "IPBWTemplateHaskellGCC"   <|> lk "IPBWTemplateHaskellRun"   <->
   "IPBW"           <|> lk "IPBWMiniWellScopedCompile"      <|> lk "IPBWMiniWellScopedGen"      <|> lk "IPBWMiniWellScopedGCC"      <|> lk "IPBWMiniWellScopedRun"      <@>
   "FFT"            <|> lk "FFTTemplateHaskellCompile"    <|> lk "FFTTemplateHaskellGen"    <|> lk "FFTTemplateHaskellGCC"    <|> lk "FFTTemplateHaskellRun"    <->
   "FFT"            <|> lk "FFTMiniWellScopedCompile"       <|> lk "FFTMiniWellScopedGen"       <|> lk "FFTMiniWellScopedGCC"       <|> lk "FFTMiniWellScopedRun"       <@>
   "CRC"            <|> lk "CRCTemplateHaskellCompile"    <|> lk "CRCTemplateHaskellGen"    <|> lk "CRCTemplateHaskellGCC"    <|> lk "CRCTemplateHaskellRun"    <->
   "CRC"            <|> lk "CRCMiniWellScopedCompile"       <|> lk "CRCMiniWellScopedGen"       <|> lk "CRCMiniWellScopedGCC"       <|> lk "CRCMiniWellScopedRun"       <@>
   "Windowing"      <|> lk "WindowingTemplateHaskellCompile" <|> lk "WindowingTemplateHaskellGen" <|> lk "WindowingTemplateHaskellGCC"
    <|> lk "WindowingTemplateHaskellRun" <->
   "Windowing"      <|> lk "WindowingMiniWellScopedCompile"    <|> lk "WindowingMiniWellScopedGen"    <|> lk "WindowingMiniWellScopedGCC"
    <|> lk "WindowingMiniWellScopedRun"
