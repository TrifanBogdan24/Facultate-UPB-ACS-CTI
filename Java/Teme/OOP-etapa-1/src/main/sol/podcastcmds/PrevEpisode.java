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

public final class PrevEpisode implements FunctionHeader {

    private PrevEpisode() {

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
        PrevEpisode obj = new PrevEpisode();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * prev (pentru podcast)
     *
     * @param library   primeste toata arhiva cu melodii, cantece si utilizatori
     *                  playlist-urile apartin utilizatorilor
     * @param inputcmd  primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                     final OutCmd outputcmd) {

                /* repeat type
        0 = No Repeat
        1 = Repeat Once
        2 = Repeat Infinite
         */

        SingletonInformation info = SingletonInformation.getInstance();

        PodcastInput loadedPodcast = info.getLoadedPodcast();
        ArrayList<EpisodeInput> episoade = loadedPodcast.getEpisodes();

        int timeEpisode = info.getTimeCurrentPodcastEpisode();
        int idxEpisode = info.getIdxCurrentPodcastEpisode();

        EpisodeInput currentEpisode;

        if (timeEpisode > 0) {
            // redam episodul de la inceput
            timeEpisode = 0;
            currentEpisode = episoade.get(idxEpisode);
            PodcastStatus.setPodcastTime(idxEpisode, timeEpisode, currentEpisode.getDuration());
            outputcmd.setMessage("Returned to previous track successfully. The current track is "
                    + currentEpisode.getName() + ".");
            return;
        }

        idxEpisode--;
        timeEpisode = 0;

        if (idxEpisode < 0) {
            idxEpisode = 0;
            currentEpisode = episoade.get(idxEpisode);
            PodcastStatus.setPodcastTime(idxEpisode, timeEpisode, currentEpisode.getDuration());
            outputcmd.setMessage("Returned to previous track successfully. The current track is "
                    + currentEpisode.getName() + ".");
            return;
        }

        currentEpisode = episoade.get(idxEpisode);
        PodcastStatus.setPodcastTime(idxEpisode, timeEpisode, currentEpisode.getDuration());
        outputcmd.setMessage("Returned to previous track successfully. The current track is "
                + currentEpisode.getName() + ".");
    }
}
