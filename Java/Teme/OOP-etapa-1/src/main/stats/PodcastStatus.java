package main.stats;


import fileio.input.PodcastInput;
import main.SingletonInformation;
import main.cmd.InCmd;
import fileio.input.EpisodeInput;

import java.util.ArrayList;

public final class PodcastStatus {


    private PodcastStatus() {

    }



    /**
     * metoda verifica si updeteaza starea podcast-ului din loader
     * in functie de care se vor seta field-uri din clasa 'SingletonInformation'
     *
     * @param inputcmd primeste comanda curenta de interpretat
     */
    public static void showPodcastStatus(final InCmd inputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();

        if (!info.isLoaded()) {
            // nu este nimic in load
            info.podcastPlayerOutOfBounds();
            return;
        }

        // actualizam timpul de redare al podcast-ului,
        // schimbam episodul curent daca este nevoie

        if (!info.isPaused()) {
            updatePodcastTime(inputcmd.getTimestamp());
            info.setPrevPodcastTimestamp(inputcmd.getTimestamp());
        }

        if (!info.isLoaded()) {
            info.podcastPlayerOutOfBounds();
            return;
        }

        if (info.getIdxCurrentPodcastEpisode() >= info.getLoadedPodcast().getEpisodes().size()) {
            info.podcastPlayerOutOfBounds();
        }
    }

    /**
     * updatam unde am ramas cu podcast-ul curent
     * updatam indexul episodului curent
     * si timpul care s-a scurs din aceasta
     *
     * @param currentTimestamp timpul curent
     */
    public static void updatePodcastTime(final int currentTimestamp) {

        SingletonInformation info = SingletonInformation.getInstance();

        PodcastInput loadedPodcast = info.getLoadedPodcast();

        if (loadedPodcast == null) {
            return;
        }

        ArrayList<EpisodeInput> episoade = loadedPodcast.getEpisodes();
        int idxEpisode = info.getIdxCurrentPodcastEpisode();
        int timeEpisode = info.getTimeCurrentPodcastEpisode();
        int lastTime = info.getPrevPodcastTimestamp();
        int timeDiff = currentTimestamp - lastTime;


        timeEpisode += timeDiff;

        while (timeEpisode >= episoade.get(idxEpisode).getDuration()) {
            timeEpisode -= episoade.get(idxEpisode).getDuration();
            idxEpisode++;

            if (idxEpisode >= episoade.size()) {
                break;
            }
        }

        if (idxEpisode >= episoade.size()) {
            info.podcastPlayerOutOfBounds();
        }


        info.setPrevPodcastTimestamp(currentTimestamp);
        info.setIdxCurrentPodcastEpisode(idxEpisode);
        info.setTimeCurrentPodcastEpisode(timeEpisode);

        info.setRemainedEpisodeTime(episoade.get(idxEpisode).getDuration() - timeEpisode);
    }


    /**
     * updateaza episodul curent din podcast
     *
     * @param idxEpisode -> indexul episodului curent din podcast
     * @param timeEpisode -> timpul scurs din episodul curent
     * @param episodeDuration -> durata episodului curent
     */
    public static void setPodcastTime(final int idxEpisode, final int timeEpisode,
                                       final int episodeDuration) {
        SingletonInformation info = SingletonInformation.getInstance();

        info.setIdxCurrentPodcastEpisode(idxEpisode);
        info.setTimeCurrentPodcastEpisode(timeEpisode);
        info.setRemainedEpisodeTime(episodeDuration - timeEpisode);
    }
}
