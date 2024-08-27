package main.stats;


import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;
import fileio.input.LibraryInput;
import fileio.input.SongInput;
import main.Playlist;

import java.util.ArrayList;

public final class PlaylistStatus {

    private PlaylistStatus() {

    }

    /**
     * metoda verifica si updeteaza starea playlist-ului din loader
     * in functie de care se vor seta field-uri din clasa 'SingletonInformation'
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public static void showPlaylistStatus(final LibraryInput library, final InCmd inputcmd,
                                          final OutCmd outputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();

        if (!info.isLoaded()) {
            // nu este nimic in load
            info.playlistPlayerOutOfBounds();
            return;
        }

        // actualizam timpul de redare al playlistul, schimbam melodia curenta daca este nevoie

        if (!info.isPaused()) {
            updatePlaylistPlayTime(library, inputcmd.getTimestamp());
            info.setPrevPlaylistTimestamp(inputcmd.getTimestamp());
        }

        if (!info.isLoaded()) {
            info.playlistPlayerOutOfBounds();
            return;
        }

        if (info.getIdxCurrentPlaylistSong() >= info.getLoadedPlaylist().getSongs().size()) {
            info.playlistPlayerOutOfBounds();
        }

    }

    /**
     * updatam unde am ramas cu playlist-ul curent
     * updatam indexul melodiei curente
     * si timpul care s-a scurs din aceasta
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                playlist-urile apartin utilizatorilor
     * @param currentTimestamp timpul curent
     */
    public static void updatePlaylistPlayTime(final LibraryInput library,
                                              final int currentTimestamp) {

        /* repat type
        0 = no repeat
        1 = repeat all
        2 = repeat current song
         */

        SingletonInformation info = SingletonInformation.getInstance();
        int repeatType = info.getRepeatType();

        switch (repeatType) {
            case 0:     // No Repeat
                updateTimeNoRepeat(currentTimestamp);
                return;
            case 1:     // Repeat All
                updateTimeRepeatAll(currentTimestamp);
                return;
            case 2:     // Repeat Current Song
                updateTimeRepeatCurrentSong(currentTimestamp);
                return;
            default:
                break;
        }
    }

    /**
     * cazul pentru No Repat
     * @param currentTimestamp -> timpul curent
     */
    public static void updateTimeNoRepeat(final int currentTimestamp) {

        SingletonInformation info = SingletonInformation.getInstance();

        int repeatType = info.getRepeatType();      // = 0 (in acest caz)

        Playlist loadedPlaylist = info.getLoadedPlaylist();
        ArrayList<SongInput> playlistSongs = loadedPlaylist.getSongs();

        int timeSong = info.getTimeCurrentPlaylistSong();
        int idxSong = info.getIdxCurrentPlaylistSong();
        int timeDiff = currentTimestamp - info.getPrevPlaylistTimestamp();
        timeSong += timeDiff;

        SongInput currentSong = playlistSongs.get(idxSong);

        // iteram prin melodiile ramase din playlist

        while (idxSong < playlistSongs.size() && timeSong >= currentSong.getDuration()) {
            currentSong = playlistSongs.get(idxSong);
            timeSong -= currentSong.getDuration();
            idxSong++;
        }

        if (idxSong >= playlistSongs.size()) {
            // toate melodiile din playlist au fost redate
            info.anulateSongPlayer();
        } else {
            setPlaylistTime(idxSong, timeSong, currentSong.getDuration());
        }
    }

    /**
     * cazul pentru Repeat Current Song
     * @param currentTimestamp -> timpul curent
     */
    public static void updateTimeRepeatCurrentSong(final int currentTimestamp) {
        SingletonInformation info = SingletonInformation.getInstance();

        int repeatType = info.getRepeatType();      // = 1 (in acest caz)

        Playlist loadedPlaylist = info.getLoadedPlaylist();
        ArrayList<SongInput> playlistSongs = loadedPlaylist.getSongs();

        int timeSong = info.getTimeCurrentPlaylistSong();
        int idxSong = info.getIdxCurrentPlaylistSong();
        int timeDiff = currentTimestamp - info.getPrevPlaylistTimestamp();
        timeSong += timeDiff;

        SongInput currentSong = playlistSongs.get(idxSong);

        if (timeSong < currentSong.getDuration()) {
            // suntem in interiroul aceleasi melodii
            setPlaylistTime(idxSong, timeSong, currentSong.getDuration());
            return;
        }

        if (info.getRepeatSongCounter() >= 0) {
            // redam melodia inca o data
            info.setRepeatSongCounter(0);
            timeSong -= currentSong.getDuration();
        }




        if (timeSong < currentSong.getDuration()) {
            // este suficient sa ream melodia si a doua oara
            setPlaylistTime(idxSong, timeSong, currentSong.getDuration());
            return;
        }

        idxSong++;

        // dupa aceea redam si alte melodii
        while (idxSong < playlistSongs.size() && timeSong >= currentSong.getDuration()) {
            currentSong = playlistSongs.get(idxSong);
            timeSong -= currentSong.getDuration();
            idxSong++;
        }

        if (idxSong >= playlistSongs.size()) {
            // am redat intreg playlist-ul
            info.anulateSongPlayer();
            return;
        }

        setPlaylistTime(idxSong, timeSong, currentSong.getDuration());

    }

    /**
     * cazul pentru Repeat All
     * @param currentTimestamp -> timpul curent
     */
    public static void updateTimeRepeatAll(final int currentTimestamp) {


        SingletonInformation info = SingletonInformation.getInstance();

        // redam playlist-ul la infinit

        int repeatType = info.getRepeatType();      // = 2 (in acest caz)


        Playlist loadedPlaylist = info.getLoadedPlaylist();
        ArrayList<SongInput> playlistSongs = loadedPlaylist.getSongs();


        int timeSong = info.getTimeCurrentPlaylistSong();
        int idxSong = info.getIdxCurrentPlaylistSong();
        int timeDiff = currentTimestamp - info.getPrevPlaylistTimestamp();
        timeSong += timeDiff;

        SongInput currentSong = playlistSongs.get(idxSong);

        while (timeSong >= currentSong.getDuration()) {
            timeSong -= currentSong.getDuration();
            idxSong++;
            if (idxSong >= playlistSongs.size()) {
                idxSong = 0;
            }
            currentSong = playlistSongs.get(idxSong);
        }

        setPlaylistTime(idxSong, timeSong, currentSong.getDuration());
    }

    /**
     * updateaza melodia curenta din playlist si timpul din aceasta
     *
     * @param idxSong -> indexul melodiei curente din playlist
     * @param timeSong -> timpul scurs din melodia curenta
     * @param songDuration -> durata melodiei curente
     */
    public static void setPlaylistTime(final int idxSong, final int timeSong,
                                       final int songDuration) {
        SingletonInformation info = SingletonInformation.getInstance();

        info.setIdxCurrentPlaylistSong(idxSong);
        info.setTimeCurrentPlaylistSong(timeSong);
        info.setRemainedTimeCurrentPlaylistSong(songDuration - timeSong);
    }


}
