package com.skyhookwireless.wktparser

import org.parboiled2._
import shapeless._
import scala.collection.immutable

class WktParser(val input: ParserInput) extends Parser {
  type CoordinateList = Seq[Coordinate]
  case class Coordinate(x: Double, y: Double, z: Option[Double])

  abstract class Geometry
  case class Point(coordinate: Coordinate) extends Geometry
  case class LineString(coordinates: CoordinateList) extends Geometry
  case class Polygon(elements: Seq[CoordinateList]) extends Geometry
  case class MultiPoint(points: Seq[Point]) extends Geometry
  case class MultiPolygon(polygons: Seq[Polygon]) extends Geometry
  case class MultiLineString(lineStrings: Seq[LineString]) extends Geometry

  implicit def WspStr(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(' ')
  }

  def ws = rule { quiet(zeroOrMore(anyOf(""" \t \n"""))) }
  def withWS(c: Char) = rule { c ~ ws }
  def withWS(s: String) = rule { s ~ ws }

  def commaSep = rule { "," }

  def emptyCoordinate[T] = rule { capture("EMPTY") ~> ((a: String) => immutable.List.empty[T]) }

  def digits = rule { oneOrMore(CharPredicate.Digit) }

  def decimalNumber: Rule1[Double] = rule { capture(digits ~ "."  ~ digits)  ~>
    ((s:String) => s.toDouble) }

  def coordinate: Rule1[Coordinate] = rule { decimalNumber ~ ws ~ decimalNumber ~ ws ~ optional(decimalNumber) ~>
    ((x: Double, y: Double, z: Option[Double]) => new Coordinate(x, y, z)) }

  def coordinateSingleton = rule { ("(" ~ coordinate ~ ")") ~> ((c: Coordinate) => c)  }

  def coordinateSequence: Rule1[Seq[Coordinate]] = rule {
    ("(" ~ oneOrMore(coordinate).separatedBy(commaSep) ~ ")") ~> ((a:Seq[Coordinate]) => a) |
    emptyCoordinate[Coordinate] ~> ((a:Seq[Coordinate]) => a)
  }

  def coordinateSequenceList: Rule1[Seq[CoordinateList]] = rule {
    "(" ~ oneOrMore(coordinateSequence) ~ ")" ~> ((a:Seq[CoordinateList]) => a) |
    emptyCoordinate[CoordinateList] ~> ((a:Seq[CoordinateList]) => a)
  }

  def point: Rule1[Point] = rule { atomic("POINT") ~ coordinateSingleton ~> Point }
  def lineString: Rule1[LineString] = rule { atomic("LINESTRING") ~ coordinateSequence ~> LineString }
  def polygon: Rule1[Polygon] = rule  { atomic("POLYGON") ~ coordinateSequenceList ~> Polygon }

  def multipolygon: Rule1[MultiPolygon] = rule {
    atomic("MULTIPOLYGON") ~
    "(" ~ oneOrMore(coordinateSequenceList) ~ ")" ~> ((a:Seq[Seq[CoordinateList]]) =>
      MultiPolygon(a.map(Polygon(_))))
  }

  def multiLineString: Rule1[MultiLineString] = rule {
    atomic("MULTILINESTRING") ~ coordinateSequenceList ~> ((a:Seq[CoordinateList]) =>
        MultiLineString(a.map(LineString(_))))
    }

  def multiPoint: Rule1[MultiPoint] = rule {
    atomic("MULTIPOINT") ~ oneOrMore(coordinateSingleton).separatedBy(commaSep) ~> ((a: Seq[Coordinate]) =>
      MultiPoint(a.map(Point(_))))
  }

  def geometry: Rule1[Geometry] = rule { point | lineString | polygon | multipolygon | multiLineString | multiPoint }

  def geometryCollection = rule { oneOrMore(geometry).separatedBy(ws) }
}