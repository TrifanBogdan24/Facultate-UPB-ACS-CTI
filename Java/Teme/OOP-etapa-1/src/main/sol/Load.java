package main.sol;

import main.FunctionHeader;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;
import fileio.input.LibraryInput;

public final class Load implements FunctionHeader {


    private Load() {

    }

    /**
     * functie 'helper' prin intermediul careia se apeleaza
     * functia implementata din interfata 'FunctionHeader'
     * fara a mai fi nevoie de se crea o instanta a clasei
     * (lucru realizat deja in interiorul functiei)
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/

     */
    public static void solve(final LibraryInput library, final InCmd inputcmd,
                             final OutCmd outputcmd) {
        Load obj = new Load();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * load
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                     final OutCmd outputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();

        if (info.getResultSelect() == null || info.getResultSelect().isEmpty()) {
            outputcmd.setMessage("Please select a source before attempting to load.");
            return;
        }

        if (info.getSelectedPlaylist() != null
                && info.getSelectedPlaylist().getSongs().isEmpty()) {
            outputcmd.setMessage("You can't load an empty audio collection!");
            return;
        }

        if (info.getSelectedPodcast() != null
                && info.getSelectedPodcast().getEpisodes().isEmpty()) {
            outputcmd.setMessage("You can't load an empty audio collection!");
            return;
        }


        info.setNameSourceToLoad(info.getResultSelect());

        info.setResultedNamesForSearch(null);
        info.setResultSelect(null);

        info.setLoadedSong(info.getSelectedSong());
        info.setLoadedPodcast(info.getSelectedPodcast());
        info.setLoadedPlaylist(info.getSelectedPlaylist());

        info.setPaused(false);
        info.setLoaded(true);
        // idxCurrentPodcast ramane nemodificat
        info.setRepeatType(0);

        int timestamp = inputcmd.getTimestamp();

        if (info.getLoadedSong() != null) {
            int duration = info.getLoadedSong().getDuration();
            info.setRemainedSongTime(duration);
            info.setCurrentSongTime(0);
            info.setRepeatSongCounter(0);
            info.setPrevSongTimestamp(timestamp);
        }

        if (info.getLoadedPlaylist() != null) {
            // ca playlistul sa fie redat de la ultima melodie redata, de la inceput
            // trebuie sa ramana la fel : PlaylistStatus.idxCurrentPlaylistSong
            info.setTimeCurrentPlaylistSong(0);         // redam melodia de la inceput
            info.setRemainedTimeCurrentPlaylistSong(0);
            info.setRepeatPlaylistSongCounter(0);
            info.setPrevPlaylistTimestamp(timestamp);
        }

        if (info.getLoadedPodcast() != null) {
            // redam acelasi episod, dar de la inceput

            // ca podcastul sa fie redat de la ultimul episod redat, de unde a ramas
            // trebuie sa ramana la fel : timeCurrentPodcastEpisode
            // trebuie sa ramana la fel : idxCurrentPodcastEpisode
            info.setCounterPodcastRepeat(0);
            info.setPrevPodcastTimestamp(timestamp);
        }

        outputcmd.setMessage("Playback loaded successfully.");
    }


}
