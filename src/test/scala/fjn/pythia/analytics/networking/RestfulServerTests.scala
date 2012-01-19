/*package fjn.pythia.networking

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 10/26/11
 * Time: 6:43 PM
 * To change this template use File | Settings | File Templates.
 */
import org.specs2.mutable.Specification

class RestfulServerTests extends  Specification {
  "starting the http restfull server" should {

      "starting and stopping the server" in {
        `testAlgorithmStart` mustEqual true
        `testAlgorithmStop` mustEqual true
      }
  }
   def `testAlgorithmStart` = {
    RestfulServerPublisher.Start();
    RestfulServerPublisher.ShutDown();
    true;
  }

  def `testAlgorithmStop` = {
    GeneralMessagingService.Stop();
    true;
  }
}*/