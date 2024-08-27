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

public final class NextEpisode implements FunctionHeader {

    private NextEpisode() {

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
        NextEpisode obj = new NextEpisode();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * next (pentru podcast)
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
        idxEpisode++;           // next episode
        timeEpisode = 0;

        if (idxEpisode < episoade.size()) {
            // ne aflam in interiour-ul podcast-ului
            currentEpisode = episoade.get(idxEpisode);
            PodcastStatus.setPodcastTime(idxEpisode, timeEpisode, currentEpisode.getDuration());
            outputcmd.setMessage("Skipped to next track successfully. The current track is "
                    + currentEpisode.getName()  + ".");
            return;
        }

        // podcast-ul a fost redat in totalitate

        if (info.getRepeatType() == 0) {
            // No Repeat
            info.podcastPlayerOutOfBounds();
            outputcmd.setMessage("Cannot skip to next.");
            return;
        } else if (info.getRepeatType() == 1) {
            // Repeat Once (redam din nou ultimul episod)
            idxEpisode = episoade.size() - 1;
            currentEpisode = episoade.get(idxEpisode);
            outputcmd.setMessage("Skipped to next track successfully. The current track is "
                    + currentEpisode.getName()  + ".");
            return;
        } else if (info.getRepeatType() == 2) {
            // Repeat Infinte
            idxEpisode = 0;
            currentEpisode = episoade.get(idxEpisode);
            outputcmd.setMessage("Skipped to next track successfully. The current track is "
                    + currentEpisode.getName()  + ".");
            return;
        }
    }

}
