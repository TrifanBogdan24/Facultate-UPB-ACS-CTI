package main.sol.playlistcmds;

import fileio.input.LibraryInput;
import fileio.input.SongInput;
import main.FunctionHeader;
import main.Playlist;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;
import main.stats.PlaylistStatus;

import java.util.ArrayList;

public final class NextSong implements FunctionHeader {

    private NextSong() {

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
        NextSong obj = new NextSong();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * next (pentru playlist)
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
        1 = Repeat All
        2 = Repeat Current Song
         */
        SingletonInformation info = SingletonInformation.getInstance();

        Playlist loadedPlaylist = info.getLoadedPlaylist();
        ArrayList<SongInput> playlistSongs = loadedPlaylist.getSongs();


        int timeSong = info.getTimeCurrentPlaylistSong();
        int idxSong = info.getIdxCurrentPlaylistSong();

        SongInput currentSong;
        idxSong++;          // next song
        timeSong = 0;

        if (idxSong < playlistSongs.size()) {
            // ne aflam in interiour-ul playlist-ului
            currentSong = playlistSongs.get(idxSong);
            PlaylistStatus.setPlaylistTime(idxSong, timeSong, currentSong.getDuration());
            outputcmd.setMessage("Skipped to next track successfully. The current track is "
                    + currentSong.getName()  + ".");
            return;
        }

        // playlist-ul a fost redat in intregime (idxSong == playlistSongs.size())

        if (info.getRepeatType() == 0) {
            // No Repeat
            info.playlistPlayerOutOfBounds();
            outputcmd.setMessage("Please load a source before skipping to the next track.");
            return;
        } else if (info.getRepeatType() == 1) {
            // Repeat All
            idxSong = 0;
            currentSong = playlistSongs.get(idxSong);
            outputcmd.setMessage("Skipped to next track successfully. The current track is "
                    + currentSong.getName()  + ".");
            return;
        } else if (info.getRepeatType() == 2) {
            // Repeat Current Song (redam din nou ultimul cantec)
            idxSong = playlistSongs.size() - 1;
            currentSong = playlistSongs.get(idxSong);
            outputcmd.setMessage("Skipped to next track successfully. The current track is "
                    + currentSong.getName()  + ".");
            return;
        }
    }
}
