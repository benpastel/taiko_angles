
library(seewave)
library(tuneR)

RATE = 44100
LEN_SECONDS = 0.39 # max length of each audio clip in seconds.  This was chosen
# to get a reasonable sustain measurement on all drums, while only
# needing to throw away 4 data points for being to short.
LEN_SAMPLES = LEN_SECONDS * RATE

periods = data.frame(rbind(
    c("kato", "attack", 0.05, 0.11),
    c("kato", "sustain", 0.17, 0.24),
    c("kato", "decay", 0.30, LEN_SECONDS),
    c("maui", "attack", 0.04, 0.10),
    c("maui", "sustain", 0.12, 0.16),
    c("maui", "decay", 0.21, LEN_SECONDS),
    c("practice", "attack", 0.03, 0.08),
    c("practice", "sustain", 0.11, 0.17),
    c("practice", "decay", 0.29, LEN_SECONDS)), stringsAsFactors=F)
colnames(periods) = c("drum", "type", "start", "end")
periods$start = as.numeric(periods$start) * RATE
periods$end = as.numeric(periods$end) * RATE

players = drums = woods = angles = volumes = NULL # indices
extremes = attacks = sustains = decays = NULL # amplitude envelope features
row = 1
for (p in c("player1", "player2", "player3", "player4")) {
    for (d in c("kato", "maui", "practice")) {
        for (w in c("oak", "maple")) {
            for (a in c("1", "2", "3", "4")) {
                for (v in c("a", "b", "c", "d")) {
                    path = paste("wave_hits/", p, "_", d, "_", w, "_", a, v, ".wav", sep="")
                    if (!file.exists(path)) {
                        cat("  skipping ", path, "\n")
                        next
                    }
                    # record indices
                    players[row] <- p
                    drums[row] <- d
                    woods[row] <- w
                    angles[row] <- a
                    volumes[row] <- v

                    # find the amplitude envelope features
                    wave = readWave(path, to=LEN_SAMPLES)
                    envelope = env(wave)
                    extremes[row] = quantile(envelope, 0.98)[[1]]

                    envelope_mean <- function(for_type) {
                        start = subset(periods, drum==d & type==for_type, start)[[1]]
                        end = subset(periods, drum==d & type==for_type, end)[[1]]
                        return(mean(envelope[start:end]))
                    }
                    attacks[row] = envelope_mean("attack")
                    sustains[row] = envelope_mean("sustain")
                    decays[row] = envelope_mean("decay")

                    row <- row + 1
                }
            }
        }
    }
}
auds$Players <- as.factor(players)
auds$Drums <- as.factor(drums)
auds$Woods <- as.factor(woods)
auds$Angles <- as.factor(angles)
auds$Volumes <- as.factor(volumes)

# Control for extremes separately for each drum
# Use angles == 2 as the reference
is_kato = ifelse(drums == "kato", 1, 0)
is_maui = ifelse(drums == "maui", 1, 0)
is_practice = ifelse(drums == "practice", 1, 0)
angle_1 = ifelse(angles == "1", 1, 0)
angle_3 = ifelse(angles == "3", 1, 0)
angle_4 = ifelse(angles == "4", 1, 0)

# TODO: upgrade these to pictures!!
# print("Extremes vs angles:")
# model <- lm(extremes ~ angle_1 + angle_3 + angle_4)
# print(summary(model))

# print("Attack envelope magnitude, no controls:")
# model <- lm(attacks ~ angle_1 + angle_3 + angle_4)
# print(summary(model))

# print("Sustain envelope magnitude, no controls:")
# model <- lm(sustains ~ angle_1 + angle_3 + angle_4)
# print(summary(model))

# print("Decay envelope magnitude, no controls:")
# model <- lm(decays ~ angle_1 + angle_3 + angle_4)
# print(summary(model))
 
print("Attack envelope magnitude, controlling for volume:")
model <- lm(attacks ~ is_kato : poly(extremes, 3) 
                    + is_maui : poly(extremes, 3) 
                    + is_practice : poly(extremes, 3)
                    + angle_1 + angle_3 + angle_4)
print(summary(model))
 
print("Sustain envelope magnitude, controlling for volume:")
model <- lm(sustains ~ is_kato : poly(extremes, 3) 
                    + is_maui : poly(extremes, 3) 
                    + is_practice : poly(extremes, 3)
                    + angle_1 + angle_3 + angle_4)
print(summary(model))

print("Decay envelope magnitude, controlling for volume:")
model <- lm(decays ~ is_kato : poly(extremes, 3) 
                    + is_maui : poly(extremes, 3) 
                    + is_practice : poly(extremes, 3)
                    + angle_1 + angle_3 + angle_4)
print(summary(model))


