package de.kdi.tmoloch	

import org.jsoup._
import java.net.URL
import org.jsoup._
import hhjson.JSON._

object TMoloch {

	def doGet(http : String) : java.io.InputStream = {
		new URL(http).openStream()
	}
	
	/** function which calls the rest api  
	 * ATTENTION: the order of parameters is due to curring reasons a bit unnatural
	 *			   the second parameter are the GET PARAMETERS, the last the PATH.
	 */
	def callRestAPIForJson(base : String)(get: String)(path: String) = {
		val http = base + path + "?" + get 
		println(base)
		println(get)
		println(path)
		println(http)
		
		val response = Jsoup.parse(doGet(http), "UTF-8", http)

		val body = response.body()
    
		parseJSON(body.html().replaceAll("&quot;", "\""))
	}

	case class LoginInfo(host : String, port : Int, admin : String, pw : Array[Char]){

		def callRestAPIForJsonWithBase  = callRestAPIForJson(base) _
	
		def callRestAPIForJsonWithBaseAndToken(get: String)(path: String) = callRestAPIForJsonWithBase(get + s"&token=${token}")(path)
		
		def callRestAPIForJsonWithBaseAndTokenWithEmptyGet = callRestAPIForJsonWithBaseAndToken("") _
	
		lazy val base = s"http://${host}:${port}"
		lazy val token = initToken		
		
		def initToken : String = {
			val jsonResponse = callRestAPIForJsonWithBase("")(s"/token/create/${admin}/${pw.mkString}")
			val status : String = jsonResponse("status").toString()
			status match {
				case "200" => jsonResponse("token").toString()
				case _ => {
					val error = jsonResponse("error").toString()
					throw new RuntimeException(s"There was an error aquiring the auth token (status : ${status}): ${error}")
				}
				
		    }
		}
	}
	
	var info : LoginInfo = null
	
	def apply(args : Array[String]) = {
		info = if(args exists( _ equals "-i")){ // ask user for value because of occurence of -i			
			val s = new java.util.Scanner(System.in)
			def suggestAndReadIn(suggestion : String) : String = {
				print(suggestion)
				s.next
			}	
			try { 
				LoginInfo(suggestAndReadIn("Host (without protocol): "),
						  suggestAndReadIn("Port: ").toInt, 
					   	  suggestAndReadIn("admin terraria user: "), 
						  suggestAndReadIn("password: ").toArray)
			} finally {
				s.close()
			}
		} else { // read from command line args
			val splitArgs = args map(_.split("="))
			def getOrRaise(findable : String) : String = {
				val foundPairs = splitArgs filter (_.apply(0) equals findable)
				val countFoundPairs = foundPairs.length;
				if(countFoundPairs == 1) {
					foundPairs(0)(1)
				} else {
					throw new RuntimeException(s"there are ${countFoundPairs} pairs of ${findable}!")
				}
					
			}
			LoginInfo(getOrRaise("--host"), getOrRaise("--port").toInt, getOrRaise("--admin"), getOrRaise("--pw").toArray)
		}
	}
	
}