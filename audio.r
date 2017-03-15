library(seewave)
library(tuneR)

RATE = 44100
LEN_SECONDS = 0.39 # max length of each audio clip in seconds.
LEN_SAMPLES = LEN_SECONDS * RATE
N = 384 - 11

# spectrogram bands [0, C_3, C_4, C_5, 100]
# define: 
#   "low" as below C_3, 
#   "med" as C_3 to C_4 (middle C),
#   "high" as above C_4
MIDDLE_C = 261.63 / 1000 # kHz
BANDS = c(0, MIDDLE_C / 2, MIDDLE_C, 1000)

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
lows = meds = highs = NULL # spectrogram features
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
                    envelope = env(wave, plot=F)
                    extremes[row] = quantile(envelope, 0.98)[[1]]

                    envelope_mean <- function(for_type) {
                        start = subset(periods, drum==d & type==for_type, start)[[1]]
                        end = subset(periods, drum==d & type==for_type, end)[[1]]
                        return(mean(envelope[start:end]))
                    }
                    attacks[row] = envelope_mean("attack")
                    sustains[row] = envelope_mean("sustain")
                    decays[row] = envelope_mean("decay")

                    ratios = fbands(spec(wave, plot=F), bands=BANDS, plot=F)[,2]
                    stopifnot(length(ratios) == 3)

                    lows[row] = ratios[1]
                    meds[row] = ratios[2]
                    highs[row] = ratios[3]

                    row <- row + 1
                }
            }
        }
    }
}

# Control for extremes separately for each drum
# Use angles == 2 as the reference
is_kato = ifelse(drums == "kato", 1, 0)
is_maui = ifelse(drums == "maui", 1, 0)
is_practice = ifelse(drums == "practice", 1, 0)
angle_1 = ifelse(angles == "1", 1, 0)
angle_3 = ifelse(angles == "3", 1, 0)
angle_4 = ifelse(angles == "4", 1, 0)

# TODO: upgrade these to pictures??
# print("Extremes vs angles:")
# model <- lm(extremes ~ angle_1 + angle_3 + angle_4)
# print(summary(model))

# print("Attack envelope magnitude, basic controls only:")
# model <- lm(attacks ~ angle_1 + angle_3 + angle_4
#        + is_kato + is_maui + is_practice - 1)
# print(summary(model))

# print("Sustain envelope magnitude, no controls:")
# model <- lm(sustains ~ angle_1 + angle_3 + angle_4)
# print(summary(model))

# print("Decay envelope magnitude, no controls:")
# model <- lm(decays ~ angle_1 + angle_3 + angle_4)
# print(summary(model))
 
print("Attack envelope magnitude, controlling for volume:")
model <- lm(attacks ~ drums + drums : poly(extremes, 3) 
                    + angle_1 + angle_3 + angle_4 - 1)
print(summary(model))
 
print("Sustain envelope magnitude, controlling for volume:")
model <- lm(sustains ~ drums + drums : poly(extremes, 3) 
                    + angle_1 + angle_3 + angle_4 - 1)
print(summary(model))

print("Decay envelope magnitude, controlling for volume:")
model <- lm(decays ~ drums + drums : poly(extremes, 3) 
                    + angle_1 + angle_3 + angle_4 - 1)
print(summary(model))

print("Lows by angle:")
model <- lm(lows ~ drums + drums : poly(extremes, 3) 
    + angle_1 + angle_3 + angle_4 - 1)
print(summary(model))

print("Mediums by angle:")
model <- lm(meds ~ drums + drums : poly(extremes, 3) 
    + angle_1 + angle_3 + angle_4 - 1)
print(summary(model))

print("Highs by angle:")
model <- lm(highs ~ drums + drums : poly(extremes, 3) 
    + angle_1 + angle_3 + angle_4 - 1)
print(summary(model))

# presumably suppresses everything except the mid-range
print("Zoom in on mediums for angle 4:")
model <- lm(meds ~ drums + drums : poly(extremes, 3) 
    + drums:angle_4 - 1)
print(summary(model))

