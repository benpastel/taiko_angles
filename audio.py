import numpy as np
import librosa as lb
import librosa.display
import matplotlib.pyplot as plt
from os.path import exists
from sklearn.linear_model import LinearRegression

players = ["player1", "player2", "player3", "player4"]
drums = ["kato", "maui", "practice"]
woods = ["oak", "maple"]
angles = ["1", "2", "3", "4"]
volumes = ["a", "b", "c", "d"]

LEN_SECONDS = 0.39 # max length of each audio clip in seconds.  This was chosen
# to get a reasonable sustain measurement on all drums, while only
# needing to throw away 4 data points for being to short.

periods_by_drum = {
    "kato":
        [   (0.05, 0.11),
            (0.17, 0.24),
            (0.30, LEN_SECONDS) ],
    "maui":
        [   (0.04, 0.10),
            (0.12, 0.16),
            (0.21, LEN_SECONDS) ],
    "practice":
        [   (0.03, 0.08),
            (0.11, 0.17),
            (0.29, LEN_SECONDS) ],
}
RATE = 22050 # sample rate in frames per second
FRAMES = int(LEN_SECONDS * RATE)

aud_list = np.zeros((4, 3, 2, 4, 4), dtype=object)

auds = np.zeros((4, 3, 2, 4, 4, FRAMES), dtype=np.float32)
ok = np.zeros((4, 3, 2, 4, 4), dtype=bool)

if exists("audio_matrix.npy"):
    print "loading saved audio matrix..."
    auds = np.load("audio_matrix.npy")
else:
    print "loading audio matrix from file..."
    # load the audio files by path
    for p, player in enumerate(players):
        for d, drum in enumerate(drums):
            for w, wood in enumerate(woods):
                for a, angle in enumerate(angles):
                    for v, volume in enumerate(volumes):
                        path = "hits/%s_%s_%s_%s%s.m4a" % (player, drum, wood, angle, volume)
                        if not exists(path):
                            print path, "doesn't exist; skipping"
                            continue
                        raw_aud, rate = lb.load(path)
                        assert rate == RATE
                        if len(raw_aud) < FRAMES:
                            print path, "too short; skipping"
                            continue
                        if max(raw_aud) < 0.02:
                            print path, "inaudibly soft; skipping"
                            continue
                        ok[p, d, w, a, v] = True
                        auds[p, d, w, a, v, 0:FRAMES] = raw_aud[0:FRAMES]
    np.save("audio_matrix.npy", auds)

for d, drum in enumerate(drums):
    print drum, ":"

    # for this drum, predict attack amplitude from max amplitude & max amplitude squared
    amps = np.abs(auds[:, d, :, :, :, :])
    oks = ok[:, d, :, :, :]
    extremes = np.percentile(amps, 98, axis = -1)[oks].reshape(-1, 1)
    X = np.hstack([extremes, extremes**2])

    start = int(periods_by_drum[drum][0][0] * rate)
    end = int(periods_by_drum[drum][0][1] * rate)
    attack_means = np.mean(amps[:, :, :, :, start:end], axis = -1)

    start = int(periods_by_drum[drum][1][0] * rate)
    end = int(periods_by_drum[drum][1][1] * rate)
    sustain_means = np.mean(amps[:, :, :, :, start:end], axis = -1)

    start = int(periods_by_drum[drum][2][0] * rate)
    end = int(periods_by_drum[drum][2][1] * rate)
    decay_means = np.mean(amps[:, :, :, :, start:end], axis = -1)

    y = attack_means[oks].reshape(-1, 1)
    attack_model = LinearRegression()
    attack_model.fit(X, y)
    print "\tAttack regression score from 2nd order extremes:", attack_model.score(X, y)
    attack_res = y - attack_model.predict(X)

    y = sustain_means[oks].reshape(-1, 1)
    sustain_model = LinearRegression()
    sustain_model.fit(X, y)
    print "\tSustain regression score from 2nd order extremes:", sustain_model.score(X, y)
    sustain_res = y - sustain_model.predict(X)

    y = decay_means[oks].reshape(-1, 1)
    decay_model = LinearRegression()
    decay_model.fit(X, y)
    print "\tDecay regression score from 2nd order extremes:", decay_model.score(X, y)
    decay_res = y - decay_model.predict(X)

    # for v, volume in enumerate(volumes):
    #     print "  ", volume, ":"

    #     amps = np.abs(auds[:, d, :, :, v, :])
    #     norm = np.max(amps, axis=-1)

        
    #     attack_props = np.mean(amps[:, :, :, start:end], axis=-1) / norm
    #     print "  avg attack prop:", np.mean(attack_props[ok[:,d,:,:,v]])
    #     # attack_avg = np.mean(np.abs(auds[p, d, :, :, :, start : end]))


    
    #     # sustain_avg = np.mean(np.abs(auds[p, d, :, :, :, start : end]))
    #     sustain_props = np.mean(amps[:, :, :, start:end], axis=-1) / norm
    #     print "  avg sustain prop:", np.mean(sustain_props[ok[:,d,:,:,v]])

       
    #     # decay_avg = np.mean(np.abs(auds[p, d, :, :, :, start : end]))
    #     decay_props = np.mean(amps[:, :, :, start:end], axis=-1) / norm
    #     print "  avg decay prop:", np.mean(decay_props[ok[:,d,:,:,v]])

    #     # m = np.max(np.abs(auds[p, d, :, :, :, :]))
    #     # print "    attack prop: %.4f" % (attack_avg / float(m))
    #     # print "    sustain prop: %.4f" % (sustain_avg / float(m))
    #     # print "    decay prop: %.4f" % (decay_avg / float(m))


# well... you could try to regress on volume and take the residuals
# also try avg volume
# expect +/- 0.02 ratio from player
# expect +/- 0.03 ratio from volume
