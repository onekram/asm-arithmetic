{-# LANGUAGE BangPatterns #-}

import Text.Read
import Data.Either
import Data.Semigroup ((<>))

import System.IO
import System.Exit
import System.Process
import System.IO.Error
import System.Environment

import System.Random
import Options.Applicative

-------------------------------------------------------
--------------------- Data ----------------------------
-------------------------------------------------------

data Operation = Operation {
  f    :: Integer -> Integer -> Integer,
  name :: String
}

data Test = Test {
  ord  :: Int,
  lhs  :: Integer,
  rhs  :: Integer
}

data TestResolution = TestResolution {
  ind :: Int,
  res :: Either String ()
}

instance Show TestResolution where
  show t = prefix ++ suffix (res t)
    where
      prefix = "Test " ++ show (ind t) ++ ". "
      suffix (Right _)  = "OK"
      suffix (Left err) = "ERROR: " ++ err

success :: TestResolution -> Bool
success = isRight . res

instance Show Operation where
  show = name

-------------------------------------------------------
-------------------- Testing --------------------------
-------------------------------------------------------

runSolution :: FilePath -> IO (Handle, Handle, Handle, ProcessHandle)
runSolution path = runInteractiveProcess path [] Nothing Nothing

getStudentAnswer :: FilePath -> Integer -> Integer -> IO (Either String Integer)
getStudentAnswer solution a b = getInternal >>= (\(ans, exitCode) -> pure (checkIO ans >>= checkExitCode exitCode >>= readEither))
  where
    getInternal = do (inp, out, _, pid) <- runSolution solution
                     hPutStr inp (show a ++ "\n" ++ show b ++ "\n")
                     hFlush inp
                     !ans      <- tryIOError $ hGetLine out
                     !exitCode <- waitForProcess pid
                     pure (ans, exitCode)
    checkIO ans = either (makeError "Cannot read solution's output: ") pure ans
    checkExitCode exitCode ans = case exitCode of
                                   ExitSuccess      -> pure ans
                                   ExitFailure code -> makeError "Solution's exit code != 0, is " code
    makeError prefix err = Left (prefix ++ show err)

test :: FilePath -> Operation -> Test -> IO TestResolution
test solution op t = do ans <- getStudentAnswer solution l r
                        let realAns = (f op) l r
                        case ans of
                          Right stAns -> if realAns == stAns
                                            then endWith $ Right ()
                                            else endWith $ Left (  "\n" 
                                                                ++ show l
                                                                ++ "\n" ++ show op ++ "\n"
                                                                ++ show r
                                                                ++ "\n= (real answer)\n"
                                                                ++ show realAns
                                                                ++ "\n!= (your answer)\n" 
                                                                ++ show stAns
                                                                )
                          Left err  -> endWith $ Left err
                     where
                       endWith = pure . TestResolution (ord t)
                       l       = lhs t
                       r       = rhs t

randomTest :: FilePath -> Operation -> (Integer, Integer) -> Int -> IO TestResolution
randomTest solution op minmax ord = do l <- gen
                                       r <- gen
                                       test solution op (Test ord (Prelude.max l r) (Prelude.min l r))
                                    where gen = randomRIO minmax

exam :: Options -> IO ()
exam (Options op solution m n) = generate >>= run >>= print >>= exit
  where
    minmax   = (0, 2 ^ (m * 64) - 1)
    generate = pure [1 .. n]
    run      = mapM (randomTest solution op minmax)
    print    = mapM (\x -> (putStrLn (show x) >> pure x))
    exit ts  = case all success ts of
                 True  -> exitWith ExitSuccess
                 False -> exitWith (ExitFailure 1)

-------------------------------------------------------
-------------------- Options --------------------------
-------------------------------------------------------

data Options = Options {
  operation  :: Operation,
  binary     :: String,
  max        :: Integer,
  tests      :: Int
}

options :: Parser Options
options = Options <$> op <*> binary <*> max <*> tests
  where
    binary = strOption ( long "bin" <> metavar "binary" <> help "Path to solution" )
    max    = option auto ( long "max-qwords" <> metavar "n" <> help "Max qwords (max value will be: 2 ^ (n * 64) - 1)" <> value 128 <> showDefault)
    tests  = option auto ( long "tests" <> metavar "n" <> help "Number of tests" <> value 100 <> showDefault)
    op     = hsubparser (mul <> add <> sub)
      where
        mul    = command "mul" (info (pure (Operation (*) "*")) (progDesc "Multiplication"))
        add    = command "add" (info (pure (Operation (+) "+")) (progDesc "Add"))
        sub    = command "sub" (info (pure (Operation (-) "-")) (progDesc "Substract"))

opts :: ParserInfo Options
opts = info (options <**> helper) (fullDesc <> progDesc "" <> header "test - a program, which tests mul/sub/add solutions ")

main :: IO ()
main = execParser opts >>= exam
