

import scala.io.Source

object ReflexTXT {


  def main(args: Array[String]): Unit = {

    //Reading
    val structureElements = Source.fromResource("structure.txt").getLines.toList
    val inventory = Source.fromResource("products.txt").getLines.toList

    //Printing data
    structureElements.foreach(println(_))
    inventory.foreach(println(_))

    val example = structureSplitter("| | | counter | interface | rubric | | | 0000001HL62126 | 0000001 | HL62 | 112 | | | | size | start | end | | Movement id | | 10 | 1 | 10 | | Date | | 23 | 11 | 33 | | Picker name | | 13 | 34 | 46 | | | | | | |")

    //Example
    //println("final test")
    //readInventory("0000001HL62126A1Z2E3R4T52019-10-12T07:20:50.52Zsylvaincrespy", rawProducts)

    example.elementNames.foreach(println(_))

    val rawProducts: List[RawProduct] = structureElements.map(structureSplitter)


    val products = inventory
      .map(readInventory(_, rawProducts))
      .filter(_.elementNames.nonEmpty)

    println("Nb of product built with provided data ")
    println(products.size)

  }

  //Split and parse structure elements (names, givens values, and store these data in 3 List)
  def structureSplitter(line: String): RawProduct = {
    val groups = line
      .split("\\|\\s\\|\\s\\|")
      .toList
      .tail


    val headerNames = groups.head.split("\\|")
      .map(_.replace(" ", ""))
      .toList

    val tailGroups = groups.tail
    val values = tailGroups.head.split("\\|")
    val id = values.head.replace(" ", "")

    //All header values list
    val headerValues = List(id) ::: values
      .tail
      .map(_.replace(" ", ""))
      .toList

    val bodyNameValues = tailGroups.tail.head.split("\\|\\s\\|").toList

    val body = bodyNameValues.tail
      .map(_.substring(1))

    val bodyNames = body.sliding(1,2).toList.map(_.head)
    val bodyValues = body.tail
      .sliding(1,2).toList
      .map(_.head)
      .map(_.split("\\s\\|\\s").toList)

    //All elements name attribute list
    val elementNames: List[String] = List("id") ::: headerNames ::: bodyNames

    //Element value corresponding indexes
    val bodyValueIndexes = bodyValues
      .map(_.map(_.replace(" ", "")))  // improve regex and split to remove this heavy task
      .map(_.map(_.toInt))  // le faire en meme temps de la parsing pour eviter de tout reparcourir

    new RawProduct(elementNames = elementNames, headersValues = headerValues, bodyValueIndexes = bodyValueIndexes)
  }

  // Get en inventory line and decode with structure matching information
  def readInventory(line: String, rawProducts: List[RawProduct]): Product = {

    val inventoryID = readInventoryID(line)

    //check exist
    val exist = rawProducts
      .map(_.id)
      .map(_ == inventoryID)
      .reduce(_ || _)

    //Urgent need to improve this hard
    //find a way to check earlier or use Options
    if (exist) {
      val relatedRawProduct = getCorrespondingRawProductById(inventoryID, rawProducts)

      val indexes = relatedRawProduct.bodyValueIndexes

      val lineToSplit = line.substring(14)

      var productBodyValues: List[String] = List.empty


      indexes.foreach(l => {
        productBodyValues = lineToSplit.substring(l(1) - 1, l(2)) :: productBodyValues
      })
      productBodyValues = productBodyValues.reverse

      println()
      println("Body Values")
      productBodyValues.foreach(println(_))

      val elementValues = relatedRawProduct.headersValues ::: productBodyValues

      val elementTypes = elementValues.map(_.getClass.getGenericSuperclass)

      new Product(elementNames = relatedRawProduct.elementNames, elementValues = elementValues, elementTypes = elementTypes)
    }
    else {
      new Product(List.empty,List.empty,List.empty)
    }
  }

  def getCorrespondingRawProductById(id: String, rawProducts: List[RawProduct]): RawProduct ={
    val correspondingProducts = rawProducts
      .filter(_.id == id)
      .head

    correspondingProducts
  }

  def readInventoryID(line: String): String = {
    line.substring(0,14)
  }


}
