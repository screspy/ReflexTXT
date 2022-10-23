

class RawProduct(val elementNames: List[String], val headersValues: List[String], val bodyValueIndexes: List[List[Int]]) {

  val id = headersValues.head
}


