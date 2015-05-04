package nodescala

import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be completed") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)

    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("all should return a Future containing a list of values") {
    val all0 = Future.all(List(Future(123), Future(0)))
    assert(Await.result(all0, 1 second)  === List(123, 0))
    
    val futureFail = Future.all(List(Future(123), Future(new Exception("NO"))))
    
    futureFail onComplete {
      case Success(x) => fail()
      case Failure(e) => assert(true)
    }
  }

  test("any should return the first completed future") {
    val futures0 = List(Future {throw new Exception("No")}, Future{math.sqrt(144)})
    
    //can expect to get the square root of 16 because of the complexity of sqrt
    val futures1 = List(Future {math.sqrt(16)}, Future{math.sqrt(1000)})
    
    try {
      println(Await.result(Future.any(futures1), 10 nanos))
    } catch {
      case e: Exception => println(e.getMessage)
    }

    assert(Await.result(Future.any(futures1), 10 nanos) == 4.0)

  }

  test("simple case delay") {
    val delay = Future.delay(1 second)
    delay onComplete {
      case _ => println("completed this delay")
    }

  }

  test("delay should wait to return future for specified time") {
    val t0 = System.currentTimeMillis()

    Future.delay(1 second) onComplete {
      case _ => 
        val duration = System.currentTimeMillis()-t0
        assert( duration >= 1000L && duration < 1100L)
    }

  }

  test("now should return the future only if it is ready right now") {
    val future0 = Future(123)
    assert(future0.now === 123)

    intercept[Exception]{ 
      //definitely should not be ready right away.
      Future(math.sqrt(10000)).now 
    }
  }

  test("Simple case for continueWith.") {
    
    val cont = (f: Future[Int]) => {
      val x = Await.result(f, 2 seconds)
      (x + 1).toString
    }

    val p = Promise[Int]().complete(Success(5)).future.continueWith(cont)

    p onComplete {
      case Success(x) => assert( x === "6")
      case Failure(e) => fail(e)
    }

  }

  test("continueWith of future that never completes should time out") {

    val cont = (f: Future[Int]) => {
      val x = Await.result(f, 2 seconds)
      (x + 1).toString
    }

    val f = Future.never

    f.continueWith(cont) onComplete{
      case Success(x) => fail("didn't time out")
      case Failure(e) => assert(true)
    }

  }

  test("Simple case for continue with successfully completed future") {

    val cont = (f: Try[Int]) => {
      f match {
        case Success(x) => (x+1).toString
        case Failure(e) => (-1).toString
      }
    }

    val p = Promise[Int]().complete(Success(5)).future.continue(cont)

    p onComplete {
      case Success(x) => assert(x === "6")
      case Failure(e) => fail()
    }

  }

  test ("Simple case for continue with a failed future") {

    val cont = (f: Try[Int]) => {
      f match {
        case Success(x) => (x+1).toString
        case Failure(e) => (-1).toString
      }
    }

    val p = Future.never.continue(cont)

    p onComplete {
      case Success(x) => fail()
      case Failure(e) => assert(true)
    }

  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  test("run") {

    val working = Future.run() { ct =>
      Future {
        var count = 0
        while (ct.nonCancelled) {
          println("working")
          Thread.sleep(100)
        }
        println("done")
      }
    }

    //working.unsubscribe()

    Future.delay(1 seconds) onComplete {
      case _ =>
        println("unsubscribing")
        working.unsubscribe()
    }
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }
  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




