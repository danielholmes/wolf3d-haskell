import Test.Hspec
import Wolf3D.SimSpec
import Wolf3D.RunnerSpec
import Wolf3D.GeomSpec
import Wolf3D.Display.RaySpec

main :: IO ()
main = hspec $ do
  simSpec
  runnerSpec
  geomSpec
  raySpec
