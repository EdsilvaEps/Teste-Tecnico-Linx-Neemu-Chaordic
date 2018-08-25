import scala.io.Source
import java.io._
import scala.util.matching.Regex
import play.api.libs.json._
import play.api.libs.functional.syntax._
import org.joda.time.format._
import org.joda.time._

object Main  {

  def main(args: Array[String]) {
    println("Printing json file contents")

    parseJson()



  }

  /**
  Read the provided file path and return a string Array
  with the json objects separated
  */
  def readFile(filepath : String) : Array[String] = {

    val source = Source.fromFile(filepath)
    val lines = try source.mkString finally source.close()

    val pattern = "[{](.)+[}]".r
    val patternMatch = pattern.findAllIn(lines).toArray
    return patternMatch

  }

  /* case class model for the final data we need:
    the transformed json representing each client
    and int representing their age */
  case class dataFormat(json: String, age: Int)

  // case class modelling state name and region
  case class State(state: String, region: String)

  // case class modelling one address object inside
  // the client json data
  case class Address(addressType: String,
                    city: String,
                    complement: String,
                    country: String,
                    district: String,
                    number: String,
                    state: String,
                    state_name: String,
                    region: String,
                    street: String,
                    zipcode: String
  )

  // case class modelling a single client and
  // all his/her data
  case class Client(cpf: String,
                    name: String,
                    email: String,
                    phones: Seq[String],
                    gender: String,
                    birthDate: String,
                    addresses: Seq[Address],
                    maritalStatus: String,
                    age: String
  )

  // 'Writes' method for parsing an address json object
  // to the Address case class
  implicit val addressWrites = new Writes[Address]{
    def writes(address: Address) = Json.obj(
      "addressType" -> address.addressType,
      "city" -> address.city,
      "country" -> address.country,
      "district" -> address.district,
      "number" -> address.number,
      "state" -> address.state,
      "state_name" -> address.state_name,
      "region" -> address.region,
      "street" -> address.street,
      "zipcode" -> address.zipcode
    )
  }

  // 'Writes' method for parsing a client json objct
  // into a Client case class
  implicit val clientWrites = new Writes[Client]{
    def writes(client: Client) = Json.obj(
      "cpf" -> client.cpf,
      "name" -> client.name,
      "email" -> client.email,
      "phones" -> client.phones,
      "gender" -> client.gender,
      "birthDate" -> client.birthDate,
      "addresses" -> client.addresses,
      "maritalStatus"-> client.maritalStatus,
      "age" -> client.age
    )
  }

  /**
    function performing the parsing between json string
    objects and the case classes. We also get the client age
    and the state_name/region and add to the case class

    returns dataFormat object
  */
  def parseElement(json : String) : dataFormat = {

    var myNewObj  =  Json.parse(json).as[JsObject]
    val addrs = (myNewObj \"addresses").get

    // TODO: parse all addresses
    // retrieving age and state_name/region
    val age = getAge((myNewObj \ "birthDate").as[String])
    val state = addrs(0) \ "state"
    val stateData = getState(state.as[String])

    // parsing from json to Address
    var mAddress = new Address(
          (addrs(0) \ "addressType").as[String],
          (addrs(0) \ "city").as[String],
          (addrs(0) \ "complement").as[String],
          (addrs(0) \ "country").as[String],
          (addrs(0) \ "district").as[String],
          (addrs(0) \ "number").as[String],
          (addrs(0) \ "state").as[String],
          stateData.state,
          stateData.region,
          (addrs(0) \ "street").as[String],
          (addrs(0) \ "zipcode").as[String]
    )

    val nl = List(mAddress)

    // parsing from json to Client
    var mClient = new Client(
      (myNewObj \ "cpf").as[String],
      (myNewObj \ "name").as[String],
      (myNewObj \ "email").as[String],
      (myNewObj \ "phones").get.as[Seq[String]],
      (myNewObj \ "gender").as[String],
      (myNewObj \ "birthDate").as[String],
      nl,
      (myNewObj \ "maritalStatus").as[String],
      age.toString

    )


    // parsing back to json after modifications
    val nClient = Json.toJson(mClient)

    return new dataFormat(Json.stringify(nClient), mClient.age.toInt)

  }

  // retrieves the json array of objects from every file provided and feeds them into
  // parseElement for processing, then writes the data into new files
  def parseJson()  {

    // check if our target file is available, if not create it
    val targetDirPath = "../tmp/new_data"
    val targetDir = new File(targetDirPath)
    if(!(targetDir.exists && targetDir.isDirectory)){
        targetDir.mkdir()
    }

    val dir = new File("../tmp/sample")
    if (dir.exists && dir.isDirectory) {
        // gets all files from the sample folder inside tmp
        val files  = dir.listFiles.filter(_.isFile).toList



        val pattern = "../tmp/sample".r

        var y = 0
        val dataWriter = new PrintWriter(new File("../tmp/histogram.txt"))

        for(y <- 0 to files.length - 1){

            // puts the new data on another path with the same name as
            // the file but different folder
            val newpath = pattern.replaceFirstIn(files(y).getPath, targetDirPath)
            println((y+1) + " arquivos processados")

            val objs: Array[String] = readFile(files(y).getPath)
            val jsonWriter = new PrintWriter(new File(newpath))

            // write data to files
            for(obj <- objs){
              val dataF = parseElement(obj)
              jsonWriter.write(dataF.json + "\n")
              dataWriter.write(dataF.age + "\n")


            }

            jsonWriter.close()

        }
        dataWriter.close()
        makeHistogram("../tmp/histogram.txt")

    }
  }

  // format the data collected from the files into a
  // histogram type file
  def makeHistogram(filepath : String){

    val textFile = Source.fromFile(filepath)
    val lines = try textFile.mkString finally textFile.close()

    val writer = new PrintWriter(new File(filepath))

    val counts = lines.split("\n").groupBy(identity).mapValues(_.size)
                    .toSeq.sortBy(_._1)

    for ((k,v) <- counts) writer.write(s"${k},${v}" + "\n")
    writer.close()

  }


  // select state name and region from state acronym
  def getState(state: String) : State = state match {
    case "ac" => State("acre", "norte")
    case "al" => State("alagoas", "nordeste")
    case "am" => State("amazonas", "norte")
    case "ap" => State("amapa", "norte")
    case "ba" => State("bahia", "nordeste")
    case "ce" => State("ceara", "nordeste")
    case "df" => State("distrito federal", "centro oeste")
    case "es" => State("espirito santo", "sudeste")
    case "go" => State("goias", "centro oeste")
    case "ma" => State("maranhao", "nordeste")
    case "ms" => State("mato grosso do sul", "centro oeste")
    case "mt" => State("mato grosso", "centro oeste")
    case "mg" => State("minas gerais", "sudeste")
    case "pa" => State("para", "norte")
    case "pb" => State("paraiba", "nordeste")
    case "pr" => State("parana", "sul")
    case "pe" => State("pernambuco", "nordeste")
    case "pi" => State("piaui", "nordeste")
    case "rj" => State("rio de janeiro", "sudeste")
    case "rn" => State("rio grande do norte", "nordeste")
    case "rs" => State("rio grande do sul", "sul")
    case "ro" => State("rondonia", "norte")
    case "rr" => State("roraima", "norte")
    case "sc" => State("santa catarina", "sul")
    case "sp" => State("sao paulo", "sudeste")
    case "se" => State("sergipe", "nordeste")
    case "to" => State("tocantins", "norte")

  }

  // format the birthDate string into DateTimeFormat
  // and compare with the current date time to get the age
  def getAge(bday : String) : Int = {
    val pattern = DateTimeFormat.forPattern("yyyy-MM-dd")
    val personBday = pattern.parseLocalDateTime(bday)
    val now = new LocalDateTime()
    var age = now.getYear() - personBday.getYear()
    if((personBday.getDayOfYear() > now.getDayOfYear())){
      age = age - 1
    }

    return age
  }

}
