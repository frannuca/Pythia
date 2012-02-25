import com.github.retronym.SbtOneJar
import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "fjn"
  val buildVersion      = "1.0.0"
  val buildScalaVersion = "2.9.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion//,
    //shellPrompt  := ShellPrompt.buildShellPrompt
  )
}

// Shell prompt which show the current project,
// git branch and build version
object ShellPrompt {
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f 
  }
  def currBranch = (
    ("git status -sb" lines_! devnull headOption)
      getOrElse "-" stripPrefix "## "
  )

  val buildShellPrompt = {
    (state: State) => {
      val currProject = Project.extract (state).currentProject.id
      "%s:%s:%s> ".format (
        currProject, currBranch, BuildSettings.buildVersion
      )
    }
  }
}

object Resolvers {

  val jna = "JNA" at "https://github.com/twall/jna"
  val sunrepo    = "Sun Maven2 Repo" at "http://download.java.net/maven/2"
  val sunrepoGF  = "Sun GF Maven2 Repo" at "http://download.java.net/maven/glassfish"
  val oraclerepo = "Oracle Maven2 Repo" at "http://download.oracle.com/maven"
  val googlematrixrepo = Resolver.url("mtj-google-repo",url("http://code.google.com/p/matrix-toolkits-java"))
  val apacheMathrepo = Resolver.url("apache-math-repo",url("http://commons.apache.org/math"))
  val akkarepo ="Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

  val oracleResolvers = Seq (sunrepo, sunrepoGF, oraclerepo)
  val mathResolvers = Seq(sunrepo,googlematrixrepo,apacheMathrepo,jna)
  val akkaResolver = Seq(akkarepo)
  val rsl = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots"
  val rs2 = "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"

  val scalalaResolver = Seq(rsl,rs2)

  val loggingsl4j =Resolver.url("loggingrepo",url("http://www.slf4j.org"))

  val metro = Resolver.url("Metro Web Services",url("https://metro.dev.java.net/"))




}

object Dependencies {
  val logbackVer = "0.9.16"
  val grizzlyVer = "1.9.19"

  val logbackcore    = "ch.qos.logback" % "logback-core"     % logbackVer
  val logbackclassic = "ch.qos.logback" % "logback-classic"  % logbackVer

  val jacksonjson = "org.codehaus.jackson" % "jackson-core-lgpl" % "1.7.2"

  val grizzlyframwork = "com.sun.grizzly" % "grizzly-framework" % grizzlyVer
  val grizzlyhttp     = "com.sun.grizzly" % "grizzly-http"      % grizzlyVer
  val grizzlyrcm      = "com.sun.grizzly" % "grizzly-rcm"       % grizzlyVer
  val grizzlyutils    = "com.sun.grizzly" % "grizzly-utils"     % grizzlyVer
  val grizzlyportunif = "com.sun.grizzly" % "grizzly-portunif"  % grizzlyVer



  val sleepycat = "com.sleepycat" % "je" % "4.0.92"

  val apachenet   = "commons-net"   % "commons-net"   % "2.0"
  val apachecodec = "commons-codec" % "commons-codec" % "1.4"


  val mtj = "com.googlecode.matrix-toolkits-java" % "mtj" % "0.9.14"
  val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "test"

  val specs2 =  "org.specs2" %% "specs2" % "1.5" % "test"

  val commonsMath = "org.apache.commons" % "commons-math" % "2.0"

  val akka_actor = "se.scalablesolutions.akka" % "akka-actor" % "1.2"
  val akka_remote= "se.scalablesolutions.akka" % "akka-remote" % "1.2"
  val akka_stm= "se.scalablesolutions.akka" % "akka-stm" % "1.2"

// dependencies
  val logback_core = "ch.qos.logback" % "logback-core" % "0.9.24" % "compile" //LGPL 2.1
  val logback_classic = "ch.qos.logback" % "logback-classic" % "0.9.24" % "compile" //LGPL 2.1
  val log4j_over_slf4j = "org.slf4j" % "jcl-over-slf4j" % "1.6.3"


  val Scalala =    "org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT" intransitive () withSources()
  //def Scalala =  "org.scalala" % "scalala_2.9.0-1" % "1.0.0.RC2EFG" intransitive () withSources()
  /**2D plotting library, used indirectly through Scalala */
  def JFreeCommon = "jfree" % "jcommon" % "1.0.16"

  /**2D plotting library, used indirectly through Scalala */
  def JFreeChart = "jfree" % "jfreechart" % "1.0.13"

  /**2D plotting library, used indirectly through Scalala */
  def XmlGraphicsCommons = "org.apache.xmlgraphics" % "xmlgraphics-commons" % "1.3.1"

  /**open GL support*/
  def JOGL = "net.java.dev.jogl" % "jogl-windows-i586" % "1.1.1-rc6"

  /**Utilities for File IO */
  def CommonsIo = "commons-io" % "commons-io" % "1.4"

  /**General utilities for Java language */
  def CommonsLang = "commons-lang" % "commons-lang" % "2.4"

  /**Date and Time represenation */
  def JodaTime = "joda-time" % "joda-time" % "1.6"



 // val metroDep = "com.sun.xml.ws" % "webservices" % "2.1-b15"


  def baseDirectories = "file://C:/code/libs/jogl-1.1.1-windows-i586/lib/"


  lazy val jnalib = "net.java.dev.jna" % "jna" % "3.2.2"  //def extraJars = descendents(baseDirectories, "*.jar")

 // override def unmanagedClasspath = super.unmanagedClasspath +++ extraJars
}

object PythiaBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._




  // Sub-project specific dependencies
  val commonDeps = Seq (
    logbackcore,
    logbackclassic,
    jacksonjson,
    scalatest,
    log4j_over_slf4j,
    jnalib
  )

  val serverDeps = Seq (
    grizzlyframwork,
    grizzlyhttp,
    grizzlyrcm,
    grizzlyutils,
    grizzlyportunif,
    sleepycat,
    scalatest
  )

  val matrixDeps = Seq (mtj, scalatest,specs2)

  val pricingDeps = Seq (apachenet, apachecodec, scalatest)

  val apacheMath= Seq(commonsMath)

  val akkaDeps = Seq(akka_actor,akka_remote,akka_stm)

  val plotDeps = Seq(JOGL)

  /**
   * top layer  pythia
   */
  lazy val pythia = Project (
    "pythia",
    file ("."),
    settings = buildSettings++ SbtOneJar.oneJarSettings ++ Seq (resolvers :=  mathResolvers ++ akkaResolver ++ scalalaResolver, libraryDependencies ++= commonDeps
      ++ matrixDeps ++ apacheMath ++plotDeps ++akkaDeps  ++ Seq(Scalala,JFreeCommon,JFreeChart,XmlGraphicsCommons,CommonsIo,CommonsLang,JodaTime))
  ) //aggregate (optimizer,ia, fjn.fjn.fjn.pythia.pricers)



}
