package de.kdi.tmoloch

import org.jsoup._
import java.net.URL
import hhjson.JSON._
import TMoloch._

object MolochMode extends App {

  final val NO_MOLOCH_TEAM = "green"
  final val MOLOCH_TEAM = "red"

  def _playerlist: List[Player] = Player_.getPlayers().map(Player_.getDetailedPlayerInfo(_))
  var _currentTime: Float = 700.0f
  var _daytime: Boolean = true
  var _countDays: Int = 0
  var _moloch: Player = new Player("", "", 0)
  var _isPVP: Boolean = false
  var noMolochHintShown = false
  
  object Player_ {
    def getPlayers() = {
      val path = "/v2/players/list"
      val json = info.callRestAPIForJsonWithBaseAndTokenWithEmptyGet(path)
      val players = json.apply("players")

      (for (i <- (0 to players.length - 1)) yield {
        val player = players.apply(i)

        val playername = player.apply("nickname").toString
        val loginname = player.apply("username").toString
        val team = player.apply("team").toInt

        new Player(playername, loginname, team)
      }).toList //*/
    }

    def getDetailedPlayerInfo(player: Player) = {
      val path = "/v2/players/read"
      val get = "player=" + player.playername
      val json = info.callRestAPIForJsonWithBaseAndToken(get)(path)

      val position = json.apply("position")
      val inventory = json.apply("inventory")
      val buffs = json.apply("buffs")
      
      //println(player.playername + " has " + inventory)

      player.setReadReturn(position, inventory, buffs)
	  player
    }

    def setTeamColor(color: String) = {
      _playerlist.foreach((player: Player) => {
        setTeam(player, color)
      })
    }

    def setAllNotMolochTeam = setTeamColor(NO_MOLOCH_TEAM)

    def setTeam(player: Player, team: String) = {
      val path = "/v2/server/rawcmd"
      val get = "cmd=%2Ftteam%20" + player.playername + "%20" + team
      val json = info.callRestAPIForJsonWithBaseAndToken(get)(path)

      val response = json.apply("response")

      println(response.toString)
    }

    def getKills(player: Player) = {
      val path = "/v2/server/rawcmd"
      var name = player.loginname
      if (name == "" || name.length() < 1)
        name = player.playername
      val get = "cmd=%2Fcheck%20kills%20" + name
      val json = info.callRestAPIForJsonWithBaseAndToken(get)(path)

      val kills = json.apply("response")

      player.setKills(kills toString)
      println(player.playername + ": " + kills.toString + " Kills")
    }

    def updateKillsForAll() = {
      _playerlist.foreach((player: Player) => {
        Player_.getKills(player)
      })
    }

    def setBuff(player: Player, buff: Int, seconds: Int) = {
      val path = "/v2/server/rawcmd"
      val get = "cmd=%2Fgbuff%20" + player.playername + "%20" + buff + "%20" + seconds
      val json = info.callRestAPIForJsonWithBaseAndToken(get)(path)

      val response = json.apply("response")

      println(player.playername + ": " + response.toString)
    }

    def updatePVP() = {
	  def toggle() = {
		_isPVP = !_isPVP
        togglePVP()
        var is = "deaktiviert"
        if (_isPVP)
          is = "aktiviert"
        message("PVP wurde " + is + "! Dauer: Ein halber Tag")
		println(s"Aktuelle Zeit: ${_currentTime}")
	  }
	
      val oldTime = _currentTime
      val oldDaytime = _daytime

      World_.getTime()

      if (!oldDaytime && _daytime) //on a new day
      {
		_countDays = _countDays + 1
		 toggle()
      } else if(oldDaytime && !_daytime){// night dawn
		 if(_countDays != 0){
			toggle()
		 }
	  }
  
    }

    def togglePVP() = {
      _playerlist.foreach((player: Player) => {
    	  val path = "/v2/server/rawcmd"
	      val get = "cmd=%2Ffpvp%20" + player.playername
	      val json = info.callRestAPIForJsonWithBaseAndToken(get)(path)
	
	      val response = json.apply("response")
	
	      println(response.toString)
      })
    }

    def disablePVP() = {
      //also disable manual change
      togglePVP()
      togglePVP()
    }

    def bringSuperadminHome() {
      val path = "/v2/server/rawcmd"
      val get = "cmd=%2Fhome"
      val json = info.callRestAPIForJsonWithBaseAndToken(get)(path)

      val response = json.apply("response")

      println(response.toString)
    }

    def teleportToSuperadmin(jumper: Player) = {
      val path = "/v2/server/rawcmd"
      val get = "cmd=%2Ftphere%20" + jumper.loginname
      val json = info.callRestAPIForJsonWithBaseAndToken(get)(path)

      val response = json.apply("response")

      println(response.toString)
    }

    def bringThemAllHome() {
      bringSuperadminHome()
      _playerlist.foreach((player: Player) => {
        teleportToSuperadmin(player)
      })
    }
  }

  object World_ {
    def getTime() = {
      val path = "/world/read"
      val json = info.callRestAPIForJsonWithBaseAndTokenWithEmptyGet(path)

      val time = json.apply("time")
      val daytime = json.apply("daytime")

      _currentTime = time.toString.toFloat
      _daytime = false
      if (daytime.toString.equals("true"))
        _daytime = true

    }

    def setTime(time: String) = {
      val path = "/v2/server/rawcmd"
      val get = "cmd=%2Ftime%20" + time
      val json = info.callRestAPIForJsonWithBaseAndToken(get)(path)
    }

  }

  def main() = {
    println("Welcome to Terraria - Moloch !!!")
    println("")

    TMoloch(args)

    println("set 7 o clock")
    World_.setTime("7:00")

    println("set initial teams")
    Player_.setAllNotMolochTeam
    println("disable pvp for all")
    Player_.disablePVP()
    println("")

    //go into loop
    running()
  }

  def running() = {
    val abortCriterias: List[AbortCriteria] = TMoloch.abortcrits
    while (!abortCriterias.exists(_.isAborted(info))) {
      println("update the moloch")
      updateMoloch()

      println("update PVP")
      Player_.updatePVP()

      println("update Kills")
      Player_.updateKillsForAll()
      Thread.sleep(2000)
    }

    //freeze them all
    _playerlist.foreach((player: Player) => {
      Player_.setBuff(player, 47, 60)
    })

    Player_.bringThemAllHome()

    printHighscore()
  }

  def getMoloch(): Player = {
    //go through every Player and look if he has the specific items
    for (player <- _playerlist) {
      Player_.getDetailedPlayerInfo(player)
      var isMolock = false
      if (player.inventory.contains("Large Sapphire:1"))
        isMolock = true

      if (isMolock)
    	  return player
    }

    return new Player("", "", 0)
  }

  def updateMoloch() {
    //is there a moloch?
    val newMoloch: Player = getMoloch()
    var existingMoloch = true
    if (newMoloch.playername.equals("")) {
      existingMoloch = false
    }

    //if there was no moloch do nothing
    //or if the moloch is the same do nothing
    if (newMoloch == _moloch) {
      return
    }

    //if there is no moloch more print message to players
    if (!existingMoloch && !noMolochHintShown) {
      //make them faster
      _playerlist.foreach((player: Player) => {
        Player_.setBuff(player, 3, 30)
      })

      Player_.setAllNotMolochTeam

      message("Der Moloch ist tod, sucht seine Items! " + _moloch.position)
      message("Bitte fair sein und nur einem Spieler die Items des Moloch ueberlassen!")

      _moloch = new Player("", "", 0)
      
      noMolochHintShown = true
      
      return
    }

    //if there is a new moloch update it
    if (existingMoloch && (newMoloch != _moloch)) {
      pimpTheMoloch(newMoloch)

      Player_.setAllNotMolochTeam

      Player_.setTeam(newMoloch, MOLOCH_TEAM)

      message("Der neue Moloch ist " + newMoloch.playername + "!")

      _moloch = newMoloch
      
      noMolochHintShown = false
    }
  }

  def pimpTheMoloch(newMoloch: Player) = {
    val bufftime = 15

    Player_.setBuff(newMoloch, 3, bufftime)
    Player_.setBuff(newMoloch, 2, bufftime)
    Player_.setBuff(newMoloch, 5, bufftime)
    Player_.setBuff(newMoloch, 17, bufftime)
    Player_.setBuff(newMoloch, 62, bufftime)
    Player_.setBuff(newMoloch, 63, bufftime)
    Player_.setBuff(newMoloch, 58, bufftime)
  }

  def message(msg: String) = {
    val path = "/v2/server/broadcast"
    val get = "msg=" + (msg.replace(" ", "%20").replace("!", "%21").replace(",", "%2C").replace(":", "%3A"))
    val json = info.callRestAPIForJsonWithBaseAndToken(get)(path)

    val response = json.apply("response")

    println(response.toString + ": " + msg)
  }

  def printHighscore() {
    var array : List[(String, Int)] = (for (player <- _playerlist) yield {
      val kills = player.kills

      val index_killed = kills.indexOf("killed")
      val index_players = kills.indexOf("players")

      (player.playername, kills.substring(index_killed + 7, index_players - 1).toInt)
    }).toList

    var killist: List[(String, Int)] = array.sortWith((a, b) => {
      if (a._2 < b._2)
        false
      else if (a._2 > b._2)
        true
      else
        true
    })
    if (!killist.isEmpty) {
      message("Der beste Spieler ist " + killist(0)._1 + " mit " + killist(0)._2 + " Kills!")
      makeFirework(killist(0)._1)
    }
    if (killist.size < 2)
      return

    message("Kills der anderen Spieler:")
    for (i <- (1 to (killist.size - 1))) {
      message(killist(i)._1 + " hat " + killist(i)._2 + " Kills!")
    }
  }

  def makeFirework(playername: String) = {
    val path = "/v2/server/rawcmd"
    val get = "cmd=%2Ffirework%20" + playername
    val json = info.callRestAPIForJsonWithBaseAndToken(get)(path)
  }

  class Player(val playername: String, val loginname: String, val team: Int) {
    var position: String = ""
    var inventory: String = ""
    var buffs: String = ""
    var kills: String = ""

    def setReadReturn(p: String, i: String, b: String) = {
      this.position = p
      this.inventory = i
      this.buffs = b
    }

    def setKills(k: String) = {
      this.kills = k
    }
  }

  main()
}