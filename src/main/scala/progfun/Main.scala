package fr.esgi.al.funprog

import better.files.File

import scala.annotation.tailrec

class Lawnmower (
  val start_x: Integer,
  val start_y: Integer,
  val start_dir: Char,
  val dir: Char,
  val pos_x: Integer,
  val pos_y: Integer,
  val instructions: String
) {

  def toCsv(pos: Integer): String = {
    pos.toString + ";" + this.start_x.toString + ";" + this.start_y.toString + ";" + this.start_dir.toString + ";" + this.pos_x.toString + ";" + this.pos_y.toString + ";" + this.dir.toString + ";" + this.instructions
  }

  def toYaml: String = {
    "- debut:\n" +
      "    point:\n" +
      "      x: " + this.start_x.toString + "\n" +
      "      y: " + this.start_y.toString + "\n" +
      "    direction: " + this.start_dir.toString + "\n" +
      "  instructions:\n" +
      this.instructions.toArray.mkString("  - ", "\n  - ", "\n") +
      "  fin:\n" +
      "    point:\n" +
      "      x: " + this.pos_x.toString + "\n" +
      "      y: " + this.pos_y.toString + "\n" +
      "    direction: " + this.dir.toString + "\n"
  }

  def toJson: String = {
    "{" +
      "\"debut\": {" +
        "\"point\": {" +
          "\"x\": " + this.start_x.toString + "," +
          "\"y\": " + this.start_y.toString +
        "}," +
        "\"direction\": \"" + this.start_dir.toString + "\"" +
      "}," +
      "\"instructions\": " + this.instructions.toArray.mkString("[\"", "\",\"", "\"],") +
      "\"fin\": {" +
        "\"point\": {" +
          "\"x\": " + this.pos_x.toString + "," +
          "\"y\": " + this.pos_y.toString +
        "}," +
        "\"direction\": \"" + this.dir.toString + "\"" +
      "}" +
    "}"
  }

  def move(i: Integer, max_x: Integer, max_y: Integer): Lawnmower = {
    if (instructions.length > i) {
      (instructions.charAt(i), this.dir) match {
        case ('A', 'N') =>
          if (this.pos_y < max_y) {
            new Lawnmower(this.start_x, this.start_y, this.start_dir, this.dir, this.pos_x, this.pos_y + 1, this.instructions).move(i + 1, max_x, max_y);
          } else {
            this.move(i + 1, max_x, max_y);
          }
        case ('A', 'S') =>
          if (this.pos_y > 0) {
            new Lawnmower(this.start_x, this.start_y, this.start_dir, this.dir, this.pos_x, this.pos_y - 1, this.instructions).move(i + 1, max_x, max_y);
          } else {
            this.move(i + 1, max_x, max_y);
          }
        case ('A', 'E') =>
          if (this.pos_x < max_x) {
            new Lawnmower(this.start_x, this.start_y, this.start_dir, this.dir, this.pos_x + 1, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
          } else {
            this.move(i + 1, max_x, max_y);
          }
        case ('A', 'W') =>
          if (this.pos_x > 0) {
            new Lawnmower(this.start_x, this.start_y, this.start_dir, this.dir, this.pos_x - 1, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
          } else {
            this.move(i + 1, max_x, max_y);
          }
        case ('D', 'N') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'E', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('D', 'S') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'W', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('D', 'E') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'S', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('D', 'W') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'N', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('G', 'N') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'W', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('G', 'S') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'E', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('G', 'E') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'N', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('G', 'W') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'S', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
      }
    } else {
      this
    }
  }
}

object Main extends App {

  val x: Integer = 5;
  val y: Integer = 5;

  val lawnmowers: List[Lawnmower] = List(
    new Lawnmower(1, 2, 'N', 'N', 1, 2, "GAGAGAGAA"),
    new Lawnmower(3, 3, 'N', 'E', 3, 3, "AADAADADDA"),
  );

  val ls = computeLawnmowers(lawnmowers);

  val s_json =  gardenToJson(ls, x, y);
  val s_yaml =  gardenToYaml(ls, x, y);
  val s_csv =  gardenToCsv(ls);

  val list_conf = File("export.conf").createIfNotExists().lines.toList;

  val export_file_json = getConf("json", list_conf);
  val export_file_yaml = getConf("yaml", list_conf);
  val export_file_csv = getConf("csv", list_conf);

  export_file_json match {
    case Some(value) =>
      File(value).createIfNotExists().overwrite(s_json);
      println("Json written in : " + value);
    case None =>
      File("resources/default.json").createIfNotExists().overwrite(s_json);
      println("Json written in : resources/default.json");
  }

  export_file_yaml match {
    case Some(value) =>
      File(value).createIfNotExists().overwrite(s_yaml);
      println("Yaml written in : " + value);
    case None =>
      File("resources/default.yaml").createIfNotExists().overwrite(s_yaml);
      println("Yaml written in : resources/default.yaml");
  }

  export_file_csv match {
    case Some(value) =>
      File(value).createIfNotExists().overwrite(s_csv);
      println("CSV written in : " + value);
    case None =>
      File("resources/default.csv").createIfNotExists().overwrite(s_csv);
      println("CSV written in : resources/default.csv");
  }

  @tailrec
  def getConf(searched: String, list_conf: List[String]): Option[String] = {
    list_conf.headOption match {
      case Some(value) =>
        if (value.contains(searched)) {
          Some(value.substring(searched.length + 1, value.length));
        } else {
          getConf(searched, list_conf.drop(1));
        }
      case None =>
        None
    }
  }

  def computeLawnmowers(lawnmowers: List[Lawnmower]): List[Lawnmower] = {
    lawnmowers.headOption match {
      case Some(value) =>
        val l = value.move(0, x, y);
        val ls = computeLawnmowers(lawnmowers.drop(1));
        ls.appended(l);
      case None =>
        println("End")
        lawnmowers
    }
  }

  @tailrec
  def displayLawnmowers(lawnmowers: List[Lawnmower]): Unit = {
    lawnmowers.headOption match {
      case Some(value) =>
        println(value.pos_x.toString + " " + value.pos_y.toString + " " + value.instructions);
        displayLawnmowers(lawnmowers.drop(1));
      case None =>
        println("End")
    }
  }

  def gardenToJson(lawnmowers: List[Lawnmower], x: Integer, y: Integer): String = {
    "{" +
      "\"limite\": {" +
        "\"x\": " + x.toString + "," +
        "\"y\": " + y.toString +
      "}," +
      "\"tondeuses\": [" +
        lawnmowers.map(l => l.toJson).mkString("", ",", "") +
      "]" +
    "}";
  }

  def gardenToYaml(lawnmowers: List[Lawnmower], x: Integer, y: Integer): String = {
    "limite:\n" +
      "  x: " + x.toString + "\n" +
      "  y: " + y.toString + "\n" +
      "tondeuses:\n" +
      lawnmowers.map(l => l.toYaml).mkString("", "\n", "")
  }

  def gardenToCsv(lawnmowers: List[Lawnmower]): String = {
    "numéro;début_x;début_y;début_direction;fin_x;fin_y;fin_direction;instructions\n" +
      lawnmowers.zipWithIndex.map(value => value._1.toCsv(value._2 + 1)).mkString("", "\n", "")
  }
}
