import Test.Hspec
import Wolf3D.SimSpec
import Wolf3D.RunnerSpec
import SimEngine.GeomSpec
import SimEngine.EngineSpec

main :: IO ()
main = hspec $ do
  heroSpec
  runnerSpec
  geomSpec
  engineSpec
