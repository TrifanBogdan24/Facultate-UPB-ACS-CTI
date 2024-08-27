import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.Deque;         // pentru retinerea istoricului
import java.util.LinkedList;    // pentru retinerea istoricului


// DIAGRAM CLASSES
class DiagramComponent {
    private String text = "text";
    private String color = "WHITE";
    private int height = 40;

    private int weight = 100;

    private List<String> connectedComponents = new ArrayList<>();


    @Override
    public String toString() {
        return "[" +
                "text='" + text + '\'' +
                ", color='" + color + '\'' +
                ", height=" + height +
                ", weight=" + weight +
                ", connectedComponents=" + connectedComponents +
                ']';
    }


    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    public int getWeight() {
        return weight;
    }

    public void setWeight(int weight) {
        this.weight = weight;
    }

    public void connectTo(String componentId) {
        connectedComponents.add(componentId);
    }

    public void removeConnection(String componentId) {
        connectedComponents.remove(componentId);
    }
}


class DiagramCanvas {
    private List<DiagramComponent> components = new ArrayList<>();

    public void setComponents(final List<DiagramComponent> components) {
        this.components = components;
    }

    public List<DiagramComponent> getComponents() {
        return this.components;
    }

    public void addComponent(DiagramComponent diagramComponent) {
        components.add(diagramComponent);
    }

    public void removeComponent(DiagramComponent diagramComponent) {
        components.remove(diagramComponent);
    }

    public DiagramComponent getComponent(int id) {
        return components.get(id);
    }

    public void show() {
        System.out.println("Diagram:");
        components.forEach(System.out::println);
    }
}


// COMMAND CLASSES
enum CommandType {
    DRAW_RECTANGLE("draw rectangle"),
    CHANGE_COLOR("change color"),
    RESIZE("resize"),
    CONNECT("connect"),
    CHANGE_TEXT("change text");

    public final String text;

    CommandType(String text) {
        this.text = text;
    }

    public static CommandType fromString(String text) {
        for (CommandType commandType : CommandType.values()) {
            if (commandType.text.equalsIgnoreCase(text)) {
                return commandType;
            }
        }
        return null;
    }
}

interface DrawCommand {
    void execute();
    void undo();
}

class DrawRectangleCommand implements DrawCommand {

    private DiagramCanvas diagramCanvas;

    private Deque<DiagramComponent> diagramsHistory;
    public DrawRectangleCommand() {

    }

    public DrawRectangleCommand(DiagramCanvas diagramCanvas) {
        this.diagramCanvas = diagramCanvas;
        diagramsHistory = new LinkedList<>();
    }
    public void execute() {
        List<DiagramComponent> components = diagramCanvas.getComponents();
        DiagramComponent addedComponent = new DiagramComponent();

        components.add(addedComponent);
        diagramsHistory.addLast(addedComponent);

        diagramCanvas.setComponents(components);
    }

    @Override
    public void undo() {
        DiagramComponent component = diagramsHistory.removeLast();
        List<DiagramComponent> components = diagramCanvas.getComponents();
        components.remove(component);
        diagramCanvas.setComponents(components);
    }

    @Override
    public String toString() {
        return "DrawRectangleCommand{" +
                "diagramCanvas=" + diagramCanvas +
                '}';
    }
}



class ResizeCommand implements DrawCommand {
    private DiagramCanvas diagramCanvas;
    private Integer index;
    private Double procentage;

    private Deque<Integer> indexHistory;
    private Deque<Integer> heightHistory;
    private Deque<Integer> weightHistory;

    public ResizeCommand() {
        this.indexHistory = new LinkedList<>();
        this.heightHistory = new LinkedList<>();
        this.weightHistory = new LinkedList<>();
    }

    public ResizeCommand(final DiagramCanvas diagramCanvas, final Integer index, final  Double procentage) {
        this.diagramCanvas = diagramCanvas;
        this.index = index;
        this.procentage = procentage;
        this.indexHistory = new LinkedList<>();
        this.heightHistory = new LinkedList<>();
        this.weightHistory = new LinkedList<>();
    }

    @Override
    public void execute() {
        if (diagramCanvas.getComponents().size() < index) {
            return;
        }

        // running the actual command
        DiagramComponent component = diagramCanvas.getComponents().get(index);
        int height = component.getHeight();
        int weight = component.getWeight();

        height = (int) (procentage / 100.0 * height);
        weight = (int) (procentage / 100.0 * weight);

        component.setHeight(height);
        component.setWeight(weight);

        // adding the properties in the history
        indexHistory.addLast(index);
        heightHistory.addLast(height);
        weightHistory.addLast(weight);
    }

    @Override
    public void undo() {
        if (indexHistory.isEmpty() || heightHistory.isEmpty() || weightHistory.isEmpty()) {
            return;
        }

        // getting the properties from the last command
        Integer lastIndex = indexHistory.removeLast();
        Integer lastHeight = heightHistory.removeLast();
        Integer lastWeight = weightHistory.removeLast();

        // resetting the properties
        DiagramComponent component = diagramCanvas.getComponents().get(lastIndex);
        component.setHeight(lastHeight);
        component.setWeight(lastWeight);
    }

    @Override
    public String toString() {
        return "ResizeCommand{" +
                "diagramCanvas=" + diagramCanvas +
                ", index=" + index +
                ", procentage=" + procentage +
                '}';
    }
}

class ConnectComponentsCommand implements DrawCommand {
    private DiagramCanvas diagramCanvas;
    private String index1;
    private String index2;

    private Deque<String> historyIndex1;
    private Deque<String> historyIndex2;

    public ConnectComponentsCommand() {
        historyIndex1 = new LinkedList<>();
        historyIndex2 = new LinkedList<>();
    }

    public ConnectComponentsCommand(DiagramCanvas diagramCanvas, String index1, String index2) {
        this.diagramCanvas = diagramCanvas;
        this.index1 = index1;
        this.index2 = index2;
        historyIndex1 = new LinkedList<>();
        historyIndex2 = new LinkedList<>();
    }

    @Override
    public void execute() {
        List<DiagramComponent> components = diagramCanvas.getComponents();

        // running the actual command
        Integer idx1 = Integer.parseInt(index1);
        Integer idx2 = Integer.parseInt(index2);

        DiagramComponent component1 = components.get(idx1);
        DiagramComponent component2 = components.get(idx2);

        component1.connectTo(index2);
        component2.connectTo(index1);

        // adding properties to the history
        historyIndex1.addLast(index1);
        historyIndex2.addLast(index2);
    }

    @Override
    public void undo() {
        if (historyIndex1.isEmpty() || historyIndex2.isEmpty()) {
            return;
        }

        // getting the properties from the last command
        String lastIdx1 = historyIndex1.removeLast();
        String lastIdx2 = historyIndex2.removeLast();

        Integer idx1 = Integer.parseInt(lastIdx1);
        Integer idx2 = Integer.parseInt(lastIdx2);

        // resetting the properties
        DiagramComponent component1 = diagramCanvas.getComponents().get(idx1);
        DiagramComponent component2 = diagramCanvas.getComponents().get(idx2);

        component1.removeConnection(lastIdx2);
        component2.removeConnection(lastIdx1);
    }

    @Override
    public String toString() {
        return "ConnectComponentsCommand{" +
                "diagramCanvas=" + diagramCanvas +
                ", index1='" + index1 + '\'' +
                ", index2='" + index2 + '\'' +
                '}';
    }
}

class ChangeTextCommand implements DrawCommand {
    private DiagramCanvas diagramCanvas;
    private Integer index;
    private String text;

    private Deque<Integer> indexHistory;
    private Deque<String> textHistory;

    public ChangeTextCommand() {
        this.indexHistory = new LinkedList();
        this.textHistory = new LinkedList();
    }

    public ChangeTextCommand(DiagramCanvas diagramCanvas, Integer index, String text) {
        this.diagramCanvas = diagramCanvas;
        this.index = index;
        this.text = text;
        this.indexHistory = new LinkedList();
        this.textHistory = new LinkedList();
    }

    @Override
    public void execute() {
        if (diagramCanvas.getComponents().size() < index) {
            return;
        }

        // adding proprties to the history
        indexHistory.addLast(index);
        textHistory.addLast(diagramCanvas.getComponents().get(index).getText());

        // running the actual command
        DiagramComponent component = diagramCanvas.getComponents().get(index);
        component.setText(text);
    }

    @Override
    public void undo() {
        if (textHistory.isEmpty() || indexHistory.isEmpty()) {
            return;
        }

        // getting the properties of the last command from history
        Integer lastIndex = indexHistory.removeLast();
        String lastText = textHistory.removeLast();

        // resting the properties
        DiagramComponent component = diagramCanvas.getComponents().get(lastIndex);
        component.setText(lastText);
    }

    @Override
    public String toString() {
        return "ChangeTextCommand{" +
                "diagramCanvas=" + diagramCanvas +
                ", index=" + index +
                ", text='" + text + '\'' +
                '}';
    }
}

class ChangeColorCommand implements DrawCommand {
    private DiagramCanvas diagramCanvas;
    private Integer index;
    private String color;

    private Deque<Integer> indexHistory;
    private Deque<String> colorHistory;

    public ChangeColorCommand() {
        indexHistory = new LinkedList<>();
        colorHistory = new LinkedList<>();
    }

    public ChangeColorCommand(DiagramCanvas diagramCanvas, Integer index, String color) {
        this.diagramCanvas = diagramCanvas;
        this.index = index;
        this.color = color;
        indexHistory = new LinkedList<>();
        colorHistory = new LinkedList<>();
    }

    @Override
    public void execute() {
        if (diagramCanvas.getComponents().size() < index) {
            return;
        }

        // adding properties to the history
        indexHistory.addLast(index);
        colorHistory.addLast(diagramCanvas.getComponents().get(index).getColor());

        // running the actual command
        DiagramComponent component = diagramCanvas.getComponents().get(index);
        component.setColor(color);
    }

    @Override
    public void undo() {
        if (indexHistory.isEmpty() || colorHistory.isEmpty()) {
            return;
        }

        // getting the properties of the last command from history
        Integer lastIndex = indexHistory.removeLast();
        String lastColor = colorHistory.removeLast();

        // resetting the properties
        DiagramComponent component = diagramCanvas.getComponents().get(lastIndex);
        component.setColor(lastColor);
    }

    @Override
    public String toString() {
        return "ChangeColorCommand{" +
                "diagramCanvas=" + diagramCanvas +
                ", index=" + index +
                ", color='" + color + '\'' +
                '}';
    }
}


class Invoker {

    private Deque<DrawCommand> history;
    private Deque<DrawCommand> undos;

    /**
     * Clear up all the used resources, start fresh :D
     */
    public void restart() {
        history = new LinkedList<>();
        undos = new LinkedList<>();
    }

    /**
     * Executes a given command
     * @param command
     */
    public void execute(DrawCommand command) {
        command.execute();
        history.addLast(command);
        undos.clear();
    }

    /**
     * Undo the latest command
     */
    public void undo() {
        if (!history.isEmpty()) {
            DrawCommand command = history.removeLast();
            command.undo();
            undos.addLast(command);
        }
    }

    /**
     * Redo command previously undone. Cannot perform a redo after an execute, only after at least one undo.
     */
    public void redo() {
        if (!undos.isEmpty()) {
            DrawCommand command = undos.removeLast();
            command.execute();
        }
    }
}
class Client {

    private Invoker invoker;
    private DiagramCanvas diagramCanvas;

    Client() {
        invoker = new Invoker();
        diagramCanvas = new DiagramCanvas();
    }

    public void showDiagram() {
        diagramCanvas.show();
    }

    public void newDiagram() {
        diagramCanvas = new DiagramCanvas();
        invoker.restart();
    }

    public void executeAction(String commandName, String ...args) {
        DrawCommand command;
        try {
            CommandType commandType = CommandType.fromString(commandName);
            if (commandType == null) {
                throw new IllegalArgumentException();
            }
            command = getCommand(commandType, args);

        } catch(IllegalArgumentException ex) {
            System.out.println("Invalid command: " + commandName);
            System.out.println("Available commands:");
            for (CommandType type : CommandType.values()) {
                System.out.println("\t- " + type.text);
            }
            return;
        }

        invoker.execute(command);
    }

    private DrawCommand getCommand(CommandType type, String ...args) throws IllegalArgumentException {
        try {
            switch (type) {
                case DRAW_RECTANGLE: return new DrawRectangleCommand(diagramCanvas);
                case RESIZE: return new ResizeCommand(diagramCanvas, Integer.parseInt(args[0]), Double.parseDouble(args[1]));
                case CONNECT: return new ConnectComponentsCommand(diagramCanvas, args[0], args[1]);
                case CHANGE_TEXT: return new ChangeTextCommand(diagramCanvas, Integer.parseInt(args[0]), args[1]);
                case CHANGE_COLOR: return new ChangeColorCommand(diagramCanvas, Integer.parseInt(args[0]), args[1]);
                default: return null;
            }
        } catch (NumberFormatException ex) {
            throw new IllegalArgumentException();
        }
    }

    public void undo(){
        invoker.undo();
    }

    public void redo() {
        invoker.redo();
    }
}

public class Main {
    private static String spacerSymbols = new String(new char[40]).replace("\0", "-");

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int taskNum = scanner.nextInt();

        Client client = new Client();

        switch(taskNum) {
            case 1:
                printOutputSpacerFor("testDraw");
                testDraw(client);

                printOutputSpacerFor("testTextAndColor");
                testTextAndColor(client);

                printOutputSpacerFor("testResize");
                testResize(client);

                printOutputSpacerFor("testConnect");
                testConnect(client);

                printOutputSpacerFor("testAllCommands");
                testAllCommands(client);

                break;
            case 2:
                printOutputSpacerFor("testSimpleUndoRedo");
                testSimpleUndoRedo(client);

                printOutputSpacerFor("testComplexUndoRedo");
                testComplexUndoRedo(client);

                break;
        }

    }

    private static void printOutputSpacerFor(String test) {
        System.out.println(spacerSymbols + test + spacerSymbols);
    }

    private static void testDraw(Client client) {
        client.newDiagram();

        client.executeAction("draw rectangle");
        client.executeAction("draw rectangle");
        client.executeAction("draw rectangle");
        client.executeAction("draw rectangle");
        client.executeAction("draw rectangle");

        client.showDiagram();
    }

    private static void testTextAndColor(Client client) {
        testDraw(client);
        client.executeAction("change color", "0", "RED");
        client.executeAction("change color", "1", "BLUE");
        client.executeAction("draw rectangle");
        client.executeAction("change text", "0", "MyClass1");
        client.executeAction("change text", "5", "MyClass2");
        client.showDiagram();
    }

    private static void testConnect(Client client) {
        testDraw(client);
        client.executeAction("connect", "0", "1");
        client.executeAction("connect", "3", "2");
        client.executeAction("connect", "1", "4");
        client.executeAction("draw rectangle");
        client.showDiagram();
    }

    private static void testResize(Client client) {
        testDraw(client);
        client.executeAction("resize", "0", "10");
        client.executeAction("resize", "2", "50");
        client.executeAction("resize", "4", "25");
        client.showDiagram();
    }

    private static void testAllCommands(Client client) {
        client.newDiagram();

        client.executeAction("draw rectangle");
        client.executeAction("draw rectangle");
        client.executeAction("draw rectangle");
        client.executeAction("resize", "0", "10");
        client.executeAction("change color", "2", "PINK");
        client.executeAction("connect", "2", "1");
        client.executeAction("draw rectangle");
        client.executeAction("change text", "1", "Class1");
        client.executeAction("change text", "3", "Class2");

        client.showDiagram();
    }

    private static void testSimpleUndoRedo(Client client) {
        client.newDiagram();

        client.executeAction("draw rectangle");
        client.executeAction("change color", "0", "ORANGE");
        client.showDiagram();

        client.executeAction("draw rectangle");
        client.showDiagram();

        client.undo();
        client.showDiagram();

        client.redo();
        client.showDiagram();
    }

    private static void testComplexUndoRedo(Client client) {
        client.executeAction("draw rectangle");
        client.executeAction("draw rectangle");
        client.executeAction("draw rectangle");
        client.executeAction("resize", "0", "10");
        client.executeAction("change color", "2", "PINK");
        client.executeAction("connect", "2", "1");
        client.executeAction("draw rectangle");
        client.executeAction("change text", "1", "Class1");
        client.executeAction("change color", "3", "RED");
        client.showDiagram();
        client.undo();
        client.undo();
        client.showDiagram();

        client.undo();
        client.undo();
        client.showDiagram();

        client.redo();
        client.redo();
        client.executeAction("resize", "2", "50");
        client.showDiagram();
    }
}
