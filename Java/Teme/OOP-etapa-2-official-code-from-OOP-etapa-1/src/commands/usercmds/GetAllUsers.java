package commands.usercmds;

import app.SingletonAdmin;
import app.user.Artist;
import app.user.Host;
import app.user.User;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;

import java.util.ArrayList;

public final class GetAllUsers implements CommandRunner {

    public GetAllUsers() {

    }

    /**
     * GetAllUsers object node
     *
     * @param commandInput      the input command from the input/test* files
     * @return                  the ouput command, on object node that will be written
     *                          in the files of the result/ directory
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        ArrayList<String> result = getAllUsersResult();

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("result", objectMapper.valueToTree(result));

        return objectNode;
    }

    /**
     * GetAllUsers result
     *
     * @return                   the names of all normal users
     */
    public static ArrayList<String> getAllUsersResult() {
        SingletonAdmin admin = SingletonAdmin.getInstance();
        ArrayList<String> names = new ArrayList<>();

        for (User user: admin.getAllNormalUsers()) {
            names.add(user.getUsername());
        }

        for (Artist artist: admin.getAllArtists()) {
            names.add(artist.getUsername());
        }

        for (Host host: admin.getAllHosts()) {
            names.add(host.getUsername());
        }

        return names;
    }
}
