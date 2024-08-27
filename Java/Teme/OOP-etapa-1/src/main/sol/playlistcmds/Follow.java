package main.sol.playlistcmds;

import fileio.input.LibraryInput;
import fileio.input.UserInput;
import main.FunctionHeader;
import main.Playlist;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;

import java.util.ArrayList;

public final class Follow implements FunctionHeader {


    private Follow() {

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
        Follow obj = new Follow();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * implementeaza cerinta pentru adaugarea unui playlist la follow
     * aceasta functie updateaza o instanta de playlist,
     * modificand numele utilizatorilor care urmaresc playlist-ul
     *
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                     final OutCmd outputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();

        if (info.getResultSelect() == null) {
            outputcmd.setMessage("Please select a source before following or unfollowing.");
            return;
        }

        Playlist playlist = info.getSelectedPlaylist();

        if (playlist == null) {
            outputcmd.setMessage("The selected source is not a playlist.");
            return;
        }

        String nameFollower = inputcmd.getUsername();
        UserInput userFollower = UserInput.getUserByName(library, nameFollower);

        if (playlist.getName().equalsIgnoreCase(nameFollower)) {
            outputcmd.setMessage("You cannot follow or unfollow your own playlist.");
            return;
        }

        if (playlist.getVisibility().equalsIgnoreCase("private")) {
            outputcmd.setMessage("The selected source is not a playlist.");
            return;
        }

        ArrayList<UserInput> userFollowers = playlist.getFollowers();



        if (userFollowers.contains(userFollower)) {
            userFollowers.remove(userFollower);
            outputcmd.setMessage("Playlist unfollowed successfully.");
        } else {
            userFollowers.add(userFollower);
            outputcmd.setMessage("Playlist followed successfully.");
        }

        playlist.setFollowers(userFollowers);

        // il scoatem din select
        info.setResultSelect(null);
    }

}
