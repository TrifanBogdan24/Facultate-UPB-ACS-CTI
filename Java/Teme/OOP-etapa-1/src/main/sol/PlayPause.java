package main.sol;

import main.FunctionHeader;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;
import fileio.input.LibraryInput;

import main.stats.PlaylistStatus;
import main.stats.PodcastStatus;
import main.stats.SongStatus;

public final class PlayPause implements FunctionHeader {

    private PlayPause() {

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
        PlayPause obj = new PlayPause();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * playPause
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                     final OutCmd outputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();

        if (!info.isLoaded()) {
            outputcmd.setMessage("Please load a source,"
                    + "before attempting to pause or resume playback.");
            return;
        }

        String sourceName = info.getNameSourceToLoad();
        info.setSourceName(sourceName);

        if (!info.isPaused()) {

            if (info.getLoadedSong() != null) {
                SongStatus.updateSongPlayTime(inputcmd.getTimestamp());
            }

            if (info.getLoadedPlaylist() != null) {
                PlaylistStatus.updatePlaylistPlayTime(library, inputcmd.getTimestamp());
            }

            if (info.getLoadedPodcast() != null) {
                PodcastStatus.updatePodcastTime(inputcmd.getTimestamp());
            }

            info.setPaused(true);
            outputcmd.setMessage("Playback paused successfully.");
        } else {
            info.setPaused(false);
            int timestamp = inputcmd.getTimestamp();

            info.setPrevSongTimestamp(timestamp);
            info.setPrevPlaylistTimestamp(timestamp);
            info.setPrevPodcastTimestamp(timestamp);

            outputcmd.setMessage("Playback resumed successfully.");
        }
    }


}
