resolvers += "retronym-releases" at "http://retronym.github.com/repo/releases"

resolvers += "retronym-snapshots" at "http://retronym.github.com/repo/snapshots"

resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"


addSbtPlugin("com.github.mpeltonen" %% "sbt-idea" % "0.11.0")

addSbtPlugin("com.github.retronym" %% "sbt-onejar" % "0.7")