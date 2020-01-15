FROM hseeberger/scala-sbt
WORKDIR /rummikub
ADD . /rummikub
CMD sbt test