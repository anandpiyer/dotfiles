#!/bin/bash
 
PROJECT_NAME="$1"
ORGANIZATION_NAME="$2"
SCALA_VERSION="2.11.0"
SBT_VERSION="0.13.2"
 
mkdir $PROJECT_NAME
cd $PROJECT_NAME
 
cat > build.sbt << EOF
name := "$PROJECT_NAME"

organization := "$ORGANIZATION_NAME"

version := "1.0"
 
scalaVersion := "$SCALA_VERSION"
EOF

cat > .gitignore << EOF
target/
.DS_Store
.idea
.idea_modules
EOF
 
mkdir -p "src/main/scala"
mkdir -p "src/main/resources"
mkdir -p "src/test/scala"
mkdir -p "src/test/resources"
mkdir -p "project"

cd project

cat > build.properties << EOF
sbt.version=$SBT_VERSION
EOF

cat > plugins.sbt << EOF
resolvers ++= Seq(
        Classpaths.typesafeResolver
)

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")
EOF
 
