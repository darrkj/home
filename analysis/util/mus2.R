library(sound)
library(audio)
library(tuneR)

wav <- readMP3("rdata/oss.mp3")
wav <- readWave("rdata/pcm.wav")

plot(wav)

x <- loadSample("rdata/pcm.wav")
y <- load.wave("rdata/pcm.wav")
z <- readMP3("rdata/oss.mp3")

max(z@left)
min(z@left)

x <- z@left[1:10000]
zz <- fft(x)
plot(abs(zz))
plot(abs(zz), xlim=c(0,100))