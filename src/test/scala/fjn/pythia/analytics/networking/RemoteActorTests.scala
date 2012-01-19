/*package fjn.pythia.networking

import org.specs2.mutable.Specification

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 10/24/11
 * Time: 6:34 PM
 * To change this template use File | Settings | File Templates.
 */

class RemoteActorTests extends Specification {

  "Creating an actor test for general messages" should {

    "starting and stopping the actor remote" in {
      `testAlgorithmStart` mustEqual true
      `testAlgorithmStop` mustEqual true
    }



  }

  def `testAlgorithmStart` = {
    GeneralMessagingService.Start();
    true;
  }

  def `testAlgorithmStop` = {
    GeneralMessagingService.Stop();
    true;
  }
}*/