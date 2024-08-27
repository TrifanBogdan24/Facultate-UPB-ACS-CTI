package main.sol;

import fileio.input.LibraryInput;
import main.FunctionHeader;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;
import main.sol.playlistcmds.PrevSong;
import main.sol.podcastcmds.PrevEpisode;

public final class Prev implements FunctionHeader {

    private Prev() {

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
        Prev obj = new Prev();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * prev
     *
     * @param library   primeste toata arhiva cu melodii, cantece si utilizatori
     *                  playlist-urile apartin utilizatorilor
     * @param inputcmd  primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                     final OutCmd outputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();

        if (!info.isLoaded()) {
            outputcmd.setMessage("Please load a source before returning to the previous track.");
            return;
        }

        if (info.getLoadedSong() != null) {
            outputcmd.setMessage("The source is not a playlist / podcast.");
            return;
        }

        if (info.getLoadedPlaylist() != null) {

            PrevSong.solve(library, inputcmd, outputcmd);
            info.setPaused(false);
            info.setTimeCurrentPlaylistSong(0);
            info.setPrevPlaylistTimestamp(inputcmd.getTimestamp());

        } else if (info.getLoadedPodcast() != null) {

            PrevEpisode.solve(library, inputcmd, outputcmd);
            info.setPaused(false);
            info.setTimeCurrentPodcastEpisode(0);
            info.setPrevPodcastTimestamp(inputcmd.getTimestamp());
        }
    }
}
