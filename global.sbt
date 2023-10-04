import sys.process._

val printDependencyClasspath = taskKey[Unit]("Prints location of the dependencies")

val sourceDir = ".tag_sources/"
val tagfile = "dep_tags"
val jdkSources = "/usr/lib/jvm/temurin-11-jdk-amd64/src.zip"

printDependencyClasspath := {

  makeSourceDir()
  val classpath = (dependencyClasspath in Compile).value ++ (dependencyClasspath in Test).value
  classpath.foreach { f =>
    unzip(getSourceJar(f.data.toString))
  }
  unzip(jdkSources)
  ctagsProject()
  ctagsDeps()
}

def makeSourceDir(): Unit =
  s"mkdir -p ${sourceDir}" !

def getSourceJar(path: String): String =
  s"${path.stripSuffix(".jar")}-sources.jar"

def unzip(path: String): Unit =
  s"unzip -qq -o ${path} -d ${sourceDir}" !

def ctagsDeps(): Unit =
  s"ctags -R -f ${tagfile} --languages=scala,java ${sourceDir}" !

def ctagsProject(): Unit =
  s"ctags -R -f tags --languages=scala,java app/ test/" !
