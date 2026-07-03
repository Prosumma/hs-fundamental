import RIO
import Spec.Control
import Spec.Exceptions
import Spec.Pool
import Spec.Servant
import Spec.Textual
import Spec.Util
import Test.Hspec

main :: IO ()
main = hspec $ do
  testUtil
  testControl
  testExceptions
  testTextual
  testServant
  testPool
