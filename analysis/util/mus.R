library(sound)
library(audio)
library(tuneR)

wav <- readMP3("resource/oss.mp3")
wav <- readWave("resource/pcm.wav")

#x <- loadSample("resource/pcm.wav")
#y <- load.wave("resource/pcm.wav")
z <- readMP3("resource/oss.mp3")

max(z@left)
min(z@left)

x <- z@left[1:10000]
zz <- fft(x)
plot(abs(zz))
plot(abs(zz), xlim=c(0,100))