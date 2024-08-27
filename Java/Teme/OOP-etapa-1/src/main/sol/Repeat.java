package main.sol;

import fileio.input.LibraryInput;
import main.FunctionHeader;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;

public final class Repeat implements FunctionHeader {

    private final int maxRepeatType = 2;

    private Repeat() {

    }

    /**
     * functie 'helper' prin intermediul careia se apeleaza
     * functia implementata din interfata 'FunctionHeader'
     * fara a mai fi nevoie de se crea o instanta a clasei
     * (lucru realizat deja in interiorul functiei)
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/

     */
    public static void solve(final LibraryInput library, final InCmd inputcmd,
                             final OutCmd outputcmd) {
        Repeat obj = new Repeat();
        obj.func(library, inputcmd, outputcmd);
    }

    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * repeat
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd, final OutCmd outputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();

        if (!info.isLoaded()) {
            outputcmd.setMessage("Please load a source before setting the repeat status.");
            return;
        }


        int repeatType = info.getRepeatType() + 1;

        if (repeatType > maxRepeatType) {
            repeatType = 0;
        }

        if (repeatType == 2 && info.getLoadedPlaylist() != null) {
            info.setRepeatPlaylistSongCounter(1);
        } else if (repeatType == 1 && info.getLoadedSong() != null) {
            info.setRepeatSongCounter(1);
        } else if (repeatType == 1 && info.getLoadedPodcast() != null) {
            info.setCounterPodcastRepeat(1);
        }


        info.setRepeatType(repeatType);
        outputcmd.setMessage(info.repeatCmdMessage());

    }
}
