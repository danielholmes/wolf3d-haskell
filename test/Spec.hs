import Test.Hspec
import Wolf3D.SimSpec
import Wolf3D.RunnerSpec
import Wolf3D.GeomSpec
import Wolf3D.EngineSpec

main :: IO ()
main = hspec $ do
  simSpec
  runnerSpec
  geomSpec
  engineSpec
