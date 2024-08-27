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

public final class AddUser implements CommandRunner {

    public AddUser() {

    }

    /**
     * AddUser object node
     *
     * @param commandInput      the input command from the input/test* files
     * @return                  the ouput command, on object node that will be written
     *                          in the files of the result/ directory
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        String message = addUserMessage(commandInput);

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("user", commandInput.getUsername());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("message", message);

        return objectNode;
    }

    /**
     * AddEUser message
     *
     * @param commandInput       from the input/test* files
     * @return                   the message generated by the execution of the input command
     */
    private static String addUserMessage(final CommandInput commandInput) {
        SingletonAdmin admin = SingletonAdmin.getInstance();

        ArrayList<User> users = admin.getUsers();
        String username = commandInput.getUsername();

        if (admin.getUserByName(username) != null) {
            return "The username " + username + " is already taken.";

        }

        int age = commandInput.getAge();
        String city = commandInput.getCity();

        switch (commandInput.getType()) {
            case "host":
                Host addedHost = new Host(username, age, city);
                users.add(addedHost);
                break;
            case "artist":
                Artist addedArtist = new Artist(username, age, city);
                users.add(addedArtist);
                break;
            case "user":
                User addedUser = new User(username, age, city);
                users.add(addedUser);
                break;
            default:
                return "Invalid user type " + commandInput.getType() + ".";
        }

        admin.setUsers(users);

        return "The username " + username + " has been added successfully.";
    }

}
