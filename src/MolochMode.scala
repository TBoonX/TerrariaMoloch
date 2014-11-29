import org.jsoup._
import java.net.URL
import JSON._

object MolochMode extends App {

  final val NO_MOLOCH_TEAM = "green"
  final val MOLOCH_TEAM = "red"

  val token = "A520A66CF7F2BCA28D337FB0AA6AB5CEB89A543106761C5F954C83918E304567"

  var _playerlist: Array[Player] = Array()
  var _currentTime: Float = 700.0f
  var _daytime: Boolean = true
  var _countDays: Int = 0
  var _moloch: Player = new Player("", "", 0)
  var _isPVP: Boolean = false

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
    
    //println(html)
    
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

    //println(html)

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

    //println(html)

    val json = parseJSON(html)

    val time = json.apply("time")
    val daytime = json.apply("daytime")
    
    _currentTime = time.toString.toFloat
    _daytime = false
    if (daytime.toString.equals("true"))
    	_daytime = true
    
  }
  
  def setTeamColor(color : String) = {
    _playerlist.foreach((player: Player) => {
      setTeam(player, color)
    })
  }
  
  def setAllNotMolochTeam = setTeamColor(NO_MOLOCH_TEAM)
  
  def setTeam(player: Player, team: String) = {
    val path = "/v2/server/rawcmd"
    val get = "cmd=%2Ftteam%20" + player.playername + "%20" + team

    val ret = callRestAPI(path, get)

    val body = ret.body()
    
    var html = body.html().replaceAll("&quot;", "\"")

    //println(html)

    val json = parseJSON(html)

    val response = json.apply("response")
    
    println(response.toString)
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
    setAllNotMolochTeam
    println("disable pvp for all")
    disablePVP()
    println("")
    
    //go into loop
    running()
  }
  
  def running() = {
    while (true) {
      updateMoloch()
    }
  }
  
  def getMoloch(): Player = {
    //go through every Player and look if he has the specific items
    for (player <- _playerlist) {
      getDetailedPlayerInfo(player)
      var isMolock = false
      if (player.inventory.contains("Magic Mirror:1"))
        isMolock = true
        
      return player
    }
    
    return new Player("", "", 0)
  }
  
  def updateMoloch() {
    //is there a moloch?
    val newMoloch: Player = getMoloch()
    var existingMoloch = true
    if (newMoloch.playername.equals("")){
    	existingMoloch = false
    }
	
    //if there was no moloch do nothing
	//or if the moloch is the same do nothing
    if (newMoloch == _moloch ){
      return
    }

    //if there is no moloch more print message to players
    if (!existingMoloch) {
      message("Der Moloch ist tod, sucht seine Items! " + _moloch.position)
      message("Bitte fair sein und nur einem Spieler die Items des Moloch überlassen!")
		
      _moloch = new Player("", "", 0)
      setAllNotMolochTeam
      return
    }
    
    //if there is a new moloch update it
    if (existingMoloch && ( newMoloch != _moloch )) {
      message("Der neue Moloch ist " + newMoloch.playername + "!")
	  setAllNotMolochTeam
      setTeam(newMoloch, MOLOCH_TEAM)
      _moloch = newMoloch
    }
  }
  
  def updatePVP() = {
    val oldTime = _currentTime
    val oldDaytime = _daytime
    
    getTime()
    
    if (!oldDaytime && _daytime) {
      _isPVP = !_isPVP
      
      togglePVP()
      
      var is = "deaktiviert"
      if (_isPVP)
          is = "aktiviert"
      
       message("PVP wurde " + is + "!")
       message("Ich wiederhole: PVP wurde " + is + "!")
       message("Dauer: einen Tag")
    }
  }
  
  def togglePVP() = {
    val path = "/v2/server/rawcmd"
    val get = "cmd=%2Ffpvp%20%2A"

    val ret = callRestAPI(path, get)

    val body = ret.body()
    
    var html = body.html().replaceAll("&quot;", "\"")

    //println(html)

    val json = parseJSON(html)

    val response = json.apply("response")
    
    println(response.toString)
  }
  
  def disablePVP() = {
    //TODO
  }
  
  def message(msg: String) = {
    val path = "/v2/server/broadcast"
    val get = "msg=" + (msg.replace(" ", "%20").replace("!", "%21").replace(",", "%2C").replace(":", "%3A"))

    val ret = callRestAPI(path, get)

    val body = ret.body()
    
    var html = body.html().replaceAll("&quot;", "\"")

    //println(html)

    val json = parseJSON(html)

    val response = json.apply("response")
    
    println(response.toString)
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