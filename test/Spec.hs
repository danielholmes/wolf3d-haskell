import Test.Hspec
import Wolf3D.HeroSpec
import Wolf3D.RunnerSpec
import Wolf3D.GeomSpec
import Wolf3D.WorldSpec

main :: IO ()
main = hspec $ do
  heroSpec
  runnerSpec
  geomSpec
  worldSpec
