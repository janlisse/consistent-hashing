import java.nio.charset.Charset

import com.google.common.hash.Hashing
import scala.collection.immutable.SortedMap


class Ring private(hashedNodes: SortedMap[Int, String], val replicas: Int) {

  private val keys: Seq[Int] = hashedNodes.keys.toSeq

  def :+(node: String) = {
    new Ring(hashedNodes ++ ((1 to replicas) map { r => MurmurHash.hash(s"$r$node") -> node}),
      replicas)
  }

  def :-(node: String) = {
    new Ring(hashedNodes -- ((1 to replicas) map { r => MurmurHash.hash(s"$r$node")}),
      replicas)
  }

  def getNode(key: Array[Byte]): Option[String] = {
    if (hashedNodes.isEmpty) None
    val keyHash = MurmurHash.hash(key)
    keys.find(keyHash <= _) match {
      case Some(nodeKey) => Some(hashedNodes(nodeKey))
      case None => Some(hashedNodes(keys(0)))
    }
  }

  def empty = hashedNodes.isEmpty
}

object Ring {

  def apply(nodes: Iterable[String], replicas: Int): Ring = {
    val hashedNodes = for {
      node <- nodes
      replica <- 1 to replicas
    } yield MurmurHash.hash(s"$replica$node") -> node
    new Ring(SortedMap(hashedNodes.toArray: _*), replicas)
  }
}

object MurmurHash {
  def hash(data: String): Int = Hashing.murmur3_32().hashString(data, Charset.forName("utf-8")).asInt()
  def hash(data: Array[Byte]) = Hashing.murmur3_32().hashBytes(data).asInt()
}




