package commands.playercmds;

import app.SingletonAdmin;
import app.audio.Playlist;
import app.audio.Files.Song;
import app.player.Player;
import app.user.User;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;

import java.util.ArrayList;

public final class AddRemovePlaylist implements CommandRunner {

    public AddRemovePlaylist() {

    }

    /**
     * AddRemovePlaylist object node
     *
     * @param commandInput      the input command from the input/test* files
     * @return                  the ouput command, on object node that will be written
     *                          in the files of the result/ directory
     */
    public ObjectNode execute(final CommandInput commandInput) {

        String message = addRemoveInPlaylistMessage(commandInput);

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("user", commandInput.getUsername());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("message", message);

        return objectNode;
    }

    /**
     * AddRemovePlaylist message
     *
     * @param commandInput       from the input/test* files
     * @return                   the message generated by the execution of the input command
     */
    private static String addRemoveInPlaylistMessage(final CommandInput commandInput) {
        String username = commandInput.getUsername();

        SingletonAdmin admin = SingletonAdmin.getInstance();
        User user = admin.getUserByName(username);
        admin.setCurrentUser(user);


        if (user == null) {
            return ("The username " + username + " doesn't exist.");
        }

        user.setTimestampLastPlayedFile(null);

        if (!user.isOnline()) {
            return (username + " is offline.");
        }

        int id = commandInput.getPlaylistId();
        Player player = user.getPlayer();
        ArrayList<Playlist> playlists = user.getPlaylists();

        if (player.getCurrentAudioFile() == null) {
            return "Please load a source before adding to or removing from the playlist.";
        }

        if (player.getType().equals("podcast")) {
            return "The loaded source is not a song.";
        }

        if (id > playlists.size()) {
            return "The specified playlist does not exist.";
        }

        Playlist playlist = playlists.get(id - 1);

        if (playlist.containsSong((Song) player.getCurrentAudioFile())) {
            playlist.removeSong((Song) player.getCurrentAudioFile());
            return "Successfully removed from playlist.";
        }

        playlist.addSong((Song) player.getCurrentAudioFile());

        user.setPlaylists(playlists);
        user.setPlayer(player);
        return "Successfully added to playlist.";
    }
}
