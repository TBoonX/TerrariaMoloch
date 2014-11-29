import org.jsoup._
import java.net.URL
import JSON._

object MolochMode extends App {

  val token = "A520A66CF7F2BCA28D337FB0AA6AB5CEB89A543106761C5F954C83918E304567"

  var _playerlist: Array[Player] = Array()
  var _currentTime: Float = 700.0f
  var _daytime: Boolean = true
  var _countDays: Int = 0
  var _moloch: Player = new Player("", "", 0)

  //function which calls the rest api
  def callRestAPI(path: String, get: String): org.jsoup.nodes.Document = {
    val http = "http://192.168.0.102:7878" + path + "?" + get + "&token=" + token

    val response = Jsoup.parse(new URL(http).openStream(), "UTF-8", http)

    response
  }

  def getPlayers() = {
    val path = "/v2/players/list"
    val get = ""

    val ret = callRestAPI(path, get)

    val body = ret.body()

    var html = body.html().replaceAll("&quot;", "\"")
    
    println(html)
    
    val json = parseJSON(html)

    val players = json.apply("players")

    _playerlist = (for (i <- (0 to players.length - 1)) yield {
      val player = players.apply(i)

      val playername = player.apply("nickname").toString
      val loginname = player.apply("username").toString
      val team = player.apply("team").toInt

      new Player(playername, loginname, team)
    }).toArray//*/
  }

  def getDetailedPlayerInfo(player: Player) = {
    val path = "/v2/players/read"
    val get = "player=" + player.playername

    val ret = callRestAPI(path, get)

    val body = ret.body()
    
    var html = body.html().replaceAll("&quot;", "\"")

    println(html)

    val json = parseJSON(html)

    val position = json.apply("position")
    val inventory = json.apply("inventory")
    val buffs = json.apply("buffs")

    player.setRead(position, inventory, buffs)
  }
  
  def getTime() = {
    val path = "/world/read"
    val get = ""

    val ret = callRestAPI(path, get)

    val body = ret.body()
    
    var html = body.html().replaceAll("&quot;", "\"")

    println(html)

    val json = parseJSON(html)

    val time = json.apply("time")
    val daytime = json.apply("daytime")
    
    _currentTime = time.toString.toFloat
    _daytime = false
    if (daytime.toString.equals("true"))
    	_daytime = true
    
  }
  
  def setTeamForAll() = {
    
  }
  
  def setTeam(player: Player, team: String) = {
    val path = "/v2/server/rawcmd"
    val get = "player=" + player.playername

    val ret = callRestAPI(path, get)

    val body = ret.body()
    
    var html = body.html().replaceAll("&quot;", "\"")

    println(html)

    val json = parseJSON(html)

    val position = json.apply("position")
    val inventory = json.apply("inventory")
    val buffs = json.apply("buffs")

    player.setRead(position, inventory, buffs)
  }

  def main() = {
    println("Welcome to Terraria - Moloch !!!")
    println("")
    print("get player info")

    getPlayers
    println(" ...")
    _playerlist.foreach((player: Player) => {
    		getDetailedPlayerInfo(player)
    })

    println("get Time")
    getTime
    
    println("set initial teams")
    setTeamForAll()
    
    
  }

  class Player(val playername: String, val loginname: String, val team: Int) {
    var position: String = ""
    var inventory: String = ""
    var buffs: String = ""
    
      def setRead(p: String, i: String, b: String) = {
      	this.position = p
      	this.inventory = i
      	this.buffs = b
    }
    
    
  }

  main()
}