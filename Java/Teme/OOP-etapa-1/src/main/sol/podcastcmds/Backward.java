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

public final class Backward implements FunctionHeader {


    private final int nineteen = 90;

    private Backward() {

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
        Backward obj = new Backward();
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
            outputcmd.setMessage("Please select a source before rewinding.");
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

        timeEpisode -= nineteen;
        if (timeEpisode < 0) {
            timeEpisode = 0;
        }

        EpisodeInput currentEpisode = episodes.get(idxEpisode);
        PodcastStatus.setPodcastTime(idxEpisode, timeEpisode, currentEpisode.getDuration());
        outputcmd.setMessage("Rewound successfully.");
    }
}
