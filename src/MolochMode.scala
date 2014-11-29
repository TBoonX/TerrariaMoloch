import org.jsoup._
import java.net.URL
import JSON._

object MolochMode extends App {

  val token = "A520A66CF7F2BCA28D337FB0AA6AB5CEB89A543106761C5F954C83918E304567"

  var _playerlist: List[Player] = List()
  var _currentTime: Int = 700
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

    println(body.html())

    val json = parseJSON(body.html())

    val players = json.apply("players")
    
    players.iterator.foreach((player: ScalaJSON) => {
      val playername = player.apply("nickname")
      val loginname = player.apply("loginname")
      val team = player.apply("team")

      _playerlist = _playerlist ++ List(new Player(playername, loginname, team))
      
    })
/*
    _playerlist = (for (i <- (0 to players.length - 1)) yield {
      val player = players.apply(i)

      val playername = player.apply("nickname")
      val loginname = player.apply("loginname")
      val team = player.apply("team")

      new Player(playername, loginname, team)
    }).toArray*/
  }

  def getDetailedPlayerInfo(player: Player) = {
    val path = "/v2/players/read"
    val get = "player=" + player.playername

    val ret = callRestAPI(path, get)

    val body = ret.body()

    println(body.html())

    val json = parseJSON(body.html())

    val position = json.apply("position")
    val inventory = json.apply("inventory")
    val buffs = json.apply("buffs")

    player.setRead(position, inventory, buffs)
  }

  def main() = {
    println("Welcome to Terraria - Moloch !!!")
    println("")
    println("get player info")

    getPlayers
    _playerlist.foreach((player: Player) => {
    		getDetailedPlayerInfo(player)
    })

    
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