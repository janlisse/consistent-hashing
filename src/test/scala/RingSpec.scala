import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class RingSpec extends FlatSpec with Matchers {

  "Ring" should "add node" in {
    val ring = Ring(Nil, 3)
    val newRing = ring :+ "host3"
    val node = newRing.getNode("someKey".getBytes("utf-8"))
    node shouldBe Some("host3")
    newRing.empty shouldBe false
  }

  "Ring" should "remove node" in {
    val ring = Ring(List("host1"), 3)
    val newRing = ring :- "host1"
    newRing.empty shouldBe true
  }

  "Ring" should "be empty for Nil nodes" in {
    val ring = Ring(Nil, 3)
    ring.empty shouldBe true
  }

  "Ring" should "return None on retrieval if ring empty" in {
    val ring = Ring(Nil, 3)
    an [IndexOutOfBoundsException] should be thrownBy ring.getNode("xyz".getBytes("utf-8"))
  }
}
