package commands;

import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.statistics.GetTop5Albums;
import fileio.input.CommandInput;

import commands.usercmds.AddUser;
import commands.usercmds.DeleteUser;
import commands.usercmds.GetAllUsers;
import commands.usercmds.GetOnlineUsers;
import commands.usercmds.SwitchConnectionStatus;
import commands.usercmds.ShowPrefferedSongs;

import commands.artistcmds.AddAlbum;
import commands.artistcmds.AddEvent;
import commands.artistcmds.AddMerch;
import commands.artistcmds.RemoveAlbum;
import commands.artistcmds.RemoveEvent;
import commands.artistcmds.ShowAlbums;

import commands.hostcmds.AddAnnouncement;
import commands.hostcmds.AddPodcast;
import commands.hostcmds.RemoveAnnouncement;
import commands.hostcmds.RemovePodcast;
import commands.hostcmds.ShowPodcasts;

import commands.playercmds.AddRemovePlaylist;
import commands.playercmds.Backward;
import commands.playercmds.CreatePlaylist;
import commands.playercmds.Follow;
import commands.playercmds.Forward;
import commands.statistics.GetPreferredGenre;
import commands.statistics.GetTop5Songs;
import commands.statistics.GetTop5Playlists;
import commands.playercmds.Like;
import commands.playercmds.Load;
import commands.playercmds.Next;
import commands.playercmds.PlayPause;
import commands.playercmds.Prev;
import commands.playercmds.Repeat;
import commands.playercmds.Search;
import commands.playercmds.Select;
import commands.playercmds.ShowPlaylists;
import commands.playercmds.Shuffle;
import commands.playercmds.Status;
import commands.playercmds.SwitchVisibilty;

import commands.pagecmds.ChangePage;
import commands.pagecmds.PrintCurrentPage;


public interface CommandRunner {
    /**
     *
     * @param commandInput      input command, that will be interpreted and executed
     * @return                  ObjectNode output command
     */
    ObjectNode execute(CommandInput commandInput);

    /**
     *
     * @param command       the name of the input command
     * @return              an instance of a class that implements CommandRunner interface
     */
    static CommandRunner createCommand(String command) {

        switch (command) {
            case "search": return new Search();
            case "select": return new Select();
            case "load": return new Load();
            case "playPause": return new PlayPause();
            case "repeat": return new Repeat();
            case "shuffle": return new Shuffle();
            case "forward": return new Forward();
            case "backward": return new Backward();
            case "like": return new Like();
            case "next": return new Next();
            case "prev": return new Prev();
            case "createPlaylist": return new CreatePlaylist();
            case "addRemoveInPlaylist": return new AddRemovePlaylist();
            case "switchVisibility": return new SwitchVisibilty();
            case "showPlaylists": return new ShowPlaylists();
            case "follow": return new Follow();
            case "status": return new Status();
            case "showPreferredSongs": return new ShowPrefferedSongs();
            case "getPreferredGenre": return new GetPreferredGenre();
            case "switchConnectionStatus": return new SwitchConnectionStatus();
            case "getOnlineUsers": return new GetOnlineUsers();
            case "addUser": return new AddUser();
            case "deleteUser": return new DeleteUser();
            case "getAllUsers": return new GetAllUsers();
            case "addAlbum": return new AddAlbum();
            case "removeAlbum": return new RemoveAlbum();
            case "addEvent": return new AddEvent();
            case "removeEvent": return new RemoveEvent();
            case "addMerch": return new AddMerch();
            case "addPodcast": return new AddPodcast();
            case "removePodcast": return new RemovePodcast();
            case "addAnnouncement": return new AddAnnouncement();
            case "removeAnnouncement": return new RemoveAnnouncement();
            case "showAlbums": return new ShowAlbums();
            case "changePage": return new ChangePage();
            case "printCurrentPage": return new PrintCurrentPage();
            case "showPodcasts": return new ShowPodcasts();
            case "getTop5Songs": return new GetTop5Songs();
            case "getTop5Playlists": return new GetTop5Playlists();
            case "getTop5Albums": return new GetTop5Albums();
            default: return null;

        }
    }
}
