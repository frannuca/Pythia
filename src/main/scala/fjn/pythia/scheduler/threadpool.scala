package fjn.pythia.scheduler

import akka.actor.Actor
import akka.dispatch.Future
import ch.qos.logback.classic.pattern.ClassOfCallerConverter

/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 7/5/12
 * Time: 10:20 PM
 * To change this template use File | Settings | File Templates.
 */

class worker[T](implicit m2: Manifest[T]) extends Actor{
    def receive ={
      case action:(()=>T) =>
      {
        self.channel ! action()
      }
      case "end"=> self.stop()
      case _ => throw new Exception("Invalid message received in treadpool internal worker")
    }
  }
class threadpool[T](fs:Seq[(()=>T)],batchSize:Int,timeoutValue:Int)(implicit m2: Manifest[T]) {


  def run():Seq[T]=
  {

    println("Starting multithreaded task. No Thrd = "+batchSize.toString)
    val workers= (for (i <- 0 until batchSize) yield { akka.actor.Actor.actorOf(new worker[T])}).toSeq
    workers.foreach(w=> w.start)


    implicit val timeout = Actor.Timeout(timeoutValue)
    val futures =
    for(n <- 0 until fs.length)
      yield
    {
        workers(n%batchSize).?(fs(n))
    }
    val ret = futures.map(f => f.as[T] match{
      case Some(v) => v
      case None => throw new Exception("invalid result in actor pool function")
    })

    println("Finalizing worker threads")
    workers.foreach(w => w ! "end")

    println("Mapping results to "+ m2.getClass().getName())
    ret.map(r => r.asInstanceOf[T])
  }


}
