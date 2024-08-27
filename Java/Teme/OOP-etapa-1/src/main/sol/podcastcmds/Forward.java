package main.sol.podcastcmds;

import fileio.input.EpisodeInput;
import fileio.input.LibraryInput;
import fileio.input.PodcastInput;
import main.FunctionHeader;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;
import main.stats.PodcastStatus;

import java.util.ArrayList;

public final class Forward implements FunctionHeader {

    private final int nineteen = 90;

    private Forward() {

    }


    /**
     * functie 'helper' prin intermediul careia se apeleaza
     * functia implementata din interfata 'FunctionHeader'
     * fara a mai fi nevoie de se crea o instanta a clasei
     * (lucru realizat deja in interiorul functiei)
     *
     * @param library   primeste toata arhiva cu melodii, cantece si utilizatori
     *                  playlist-urile apartin utilizatorilor
     * @param inputcmd  primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public static void solve(final LibraryInput library, final InCmd inputcmd,
                             final OutCmd outputcmd) {
        Forward obj = new Forward();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     * <p>
     * load
     *
     * @param library   primeste toata arhiva cu melodii, cantece si utilizatori
     *                  playlist-urile apartin utilizatorilor
     * @param inputcmd  primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                     final OutCmd outputcmd) {
        /* repeat type
        0   no repat
        1   repeat once
        2   repeat infinite
         */


        SingletonInformation info = SingletonInformation.getInstance();

        if (!info.isLoaded()) {
            outputcmd.setMessage("Please load a source before attempting to forward.");
            return;
        }

        if (info.getLoadedPodcast() == null) {
            outputcmd.setMessage("The loaded source is not a podcast.");
            return;
        }

        PodcastInput loadedPodcast = info.getLoadedPodcast();
        ArrayList<EpisodeInput> episodes = loadedPodcast.getEpisodes();

        int idxEpisode = info.getIdxCurrentPodcastEpisode();
        int timeEpisode = info.getTimeCurrentPodcastEpisode();

        EpisodeInput currentEpisode = episodes.get(idxEpisode);

        timeEpisode += nineteen;
        if (timeEpisode >= currentEpisode.getDuration()) {
            timeEpisode -= currentEpisode.getDuration();
            idxEpisode++;
        }

        if (idxEpisode >= episodes.size()) {
            // TODO : to be treated for repeat
            idxEpisode = 0;
        }

        currentEpisode = episodes.get(idxEpisode);
        PodcastStatus.setPodcastTime(idxEpisode, timeEpisode, currentEpisode.getDuration());
        outputcmd.setMessage("Skipped forward successfully.");
    }



    /**
     * updateaza timpul redat din podcast
     *
     * @param repatType -> tipul de repeat
     * @param idxEpisod -> la al catelea episod am rams
     * @param playedTime -> durata redata din episod
     * @param episodDuration -> durata (integrala) episodul
     */
    public static void setPodcastTime(final int repatType, final int idxEpisod,
                                      final int playedTime, final int episodDuration) {

        SingletonInformation info = SingletonInformation.getInstance();

        info.setRepeatType(repatType);
        info.setIdxCurrentPodcastEpisode(idxEpisod);
        info.setTimeCurrentPodcastEpisode(playedTime);
        info.setRemainedEpisodeTime(episodDuration - playedTime);
    }

}
