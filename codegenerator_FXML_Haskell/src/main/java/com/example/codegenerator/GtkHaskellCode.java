package com.example.codegenerator;

import com.example.codegenerator.orientation.Orientation;
import com.example.codegenerator.relations.*;
import com.example.codegenerator.structures.*;
import com.example.codegenerator.structures.Button;
import com.example.codegenerator.structures.Label;
import com.example.codegenerator.structures.RadioButton;
import com.example.codegenerator.togglegroup.ToggleGroup;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;

public class GtkHaskellCode {
    private static Stage stage;
    private static final String username = System.getProperty("user.name"); //username of host-system-user to store .hs file
    private static final String path = "/home/"+username+"/Documents/GitHub/codegenerator_FXML_Haskell/codegenerator_FXML_Haskell/generated/gi-gtk-generated.hs";

    private static ArrayList<Relation> relations ;
    private static ArrayList<GTKWidget> GUIWidgets;
    private static ArrayList<GTKWidget> GUIContainers;
    private static ArrayList<ToggleGroup> toggleGroups;
    private static ArrayList<NotebookRelation> notebookRelations;


    private static GTKWidget currentNode; //current Node when walking trough FXML-tree

    public GtkHaskellCode(Stage stage){
        GtkHaskellCode.stage = stage;
        relations = new ArrayList<>();
        GUIWidgets = new ArrayList<>();
        GUIContainers = new ArrayList<>();
        toggleGroups = new ArrayList<>();
        notebookRelations = new ArrayList<>();
    }

    public static void generateHaskellCode(){
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(path), StandardCharsets.UTF_8))) {
            writer.write(
                    createImports() + createTopLevelWindow()+ "\n  "
            );
        }catch(IOException e){
            System.out.println(e.toString());
        }
    }

    public static String createImports(){
        String gtkHsCode = """
                {-# LANGUAGE OverloadedStrings #-}
                {-# LANGUAGE OverloadedLabels  #-}
                {-# LANGUAGE ExtendedDefaultRules #-}

                module Main (Main.main) where
                import qualified GI.Gtk as Gtk
                import GI.Gtk.Enums (WindowType(..), Orientation(..))
                import GI.Gtk (Adjustment(Adjustment))
                import Data.GI.Base ( AttrOp((:=)) )

                main :: IO ()
                main = do
                  Gtk.init Nothing
                 \s""";
        return gtkHsCode;
    }

    public static String createTopLevelWindow(){
        String stageResizableBool = String.valueOf(stage.isResizable()).substring(0, 1).toUpperCase() + String.valueOf(stage.isResizable()).substring(1);
        String createTopLevelWindow = "window <- Gtk.windowNew WindowTypeToplevel\n  ";
        String setWindowProperties =  "Gtk.set window [Gtk.windowTitle := "+"\""+stage.getTitle()+"\""+", Gtk.windowResizable := "+stageResizableBool+", Gtk.windowDefaultWidth := "+(int)stage.getWidth()+", Gtk.windowDefaultHeight := "+(int)stage.getHeight()+"]\n  ";
        String gtkHsCode = createTopLevelWindow + setWindowProperties;
        return gtkHsCode;
    }

    public static void appendTextToFile(String text){
        try {
            Files.write(Paths.get(path), text.getBytes(), StandardOpenOption.APPEND);
        }catch(IOException e){
            System.out.println(e.toString());
        }
    }

    public static void endOfFile(){
        appendTextToFile(
                generateToggleGroups() +
                generateRelations() +
                generateNotebookRelations() +
                bindTopLevelElementToWindow() +
                endMainProgram()
        );
    }

    public static String endMainProgram(){
        String onWidgetDestroy = "Gtk.onWidgetDestroy window Gtk.mainQuit\n  ";
        String widgetShowAll = "Gtk.widgetShowAll window\n  ";
        String main = "Gtk.main\n  ";
        String gtkHsCode = onWidgetDestroy+widgetShowAll+main;
        return gtkHsCode;
    }

    public static String generateRelations(){
        String relationComment = "-- Relations \n  ";
        StringBuilder relationsGtkCode = new StringBuilder();
        for(Relation r : relations){
            relationsGtkCode.append(r.generateGtkHsCode());
        }
        return relationComment + relationsGtkCode + "\n  ";
    }

    public static String generateToggleGroups(){
        ArrayList<ToggleGroup> toggleGroupsWithoutDoubles = new ArrayList<>();
        for (int i = toggleGroups.size() - 1; i >=0; i--){
            var tg = toggleGroups.get(i);
            if (!ToggleGroup.checkIfToggleExists(tg, toggleGroupsWithoutDoubles)){
                toggleGroupsWithoutDoubles.add(tg);
            }
        }

        if (!toggleGroupsWithoutDoubles.isEmpty()){
            String toggleGroupComment = "--Toggle Groups\n  ";
            StringBuilder toggleGroupGtkCode = new StringBuilder();
            for (ToggleGroup tg : toggleGroupsWithoutDoubles){
                toggleGroupGtkCode.append(tg.gtkHsCode());
            }
            return toggleGroupComment + toggleGroupGtkCode + "\n  ";
        } else {
            return "";
        }
    }

    public static String generateNotebookRelations(){
        if (!notebookRelations.isEmpty()){
            String relationComment = "--Notebook relations \n  ";
            StringBuilder relationsGtkCode = new StringBuilder();
            for(NotebookRelation r : notebookRelations){
                r.setContainerList(GUIContainers);
                r.correctChildName();
                relationsGtkCode.append(r.generateGtkHsCode());
            }
            return relationComment + relationsGtkCode + "\n  ";
        } else {
            return "";
        }

    }

    public static String bindTopLevelElementToWindow(){
        var topLevelElement = stage.getScene().getRoot();
        var hashIdTopElement = topLevelElement.hashCode();
        String topLevelBinding = "";
        for(GTKWidget q : GUIContainers){
            if(q.getId_hash().equals(hashIdTopElement)){
                topLevelBinding = "Gtk.setContainerChild window "+q.getName()+"\n  ";
            }
        }
        return topLevelBinding + "\n  ";
    }

    public static void parseGTKButton(javafx.scene.control.Button n){
        //extract corresponding attributes of JavaFX Button
        var width = n.getLayoutBounds().getWidth();
        var height = n.getLayoutBounds().getHeight();
        var layoutX = n.getLayoutX();
        var layoutY = n.getLayoutY();
        var label = ((javafx.scene.control.Button) n).getText();
        var button = new Button(n.getId(), n.hashCode(),"button", label, layoutX, layoutY, width, height);

        GUIWidgets.add(button);  //add GTKButton to control (widget) list
        appendTextToFile("-- Button \n  " + button.gtkHsCode());  //generate gi-gtk-Haskell-code for GTKButton
        currentNode = button;                                     //keep track of current Node
    }

    public static void parseGTKLayout(AnchorPane n){
        //extract corresponding attributes of JavaFX AnchorPane
        var layoutX = n.getLayoutX();
        var layoutY = n.getLayoutY();
        var layout = new Layout(n.getId(), n.hashCode(),"layout", layoutX, layoutY);

        GUIContainers.add(layout); //add GTKLayout to container list
        appendTextToFile("-- Layout \n  " + layout.gtkHsCode()); //generate gi-gtk-Haskell-code for GTKLayout
        currentNode = layout; //keep track of current Node
    }

    public static void parseGTKLabel(javafx.scene.control.Label n){
        if(!checkLabelInTab(n)) {  //if label is not from a Tab -> don't create it here, but later with the tabPane
            //extract corresponding attributes of JavaFX Label
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var text = ((javafx.scene.control.Label) n).getText();
            var label = new Label(n.getId(), n.hashCode(), "label", text, layoutX, layoutY);

            GUIWidgets.add(label); //add GTKLabel to control (widget) list
            appendTextToFile("--Label \n  " + label.gtkHsCode()); //generate gi-gtk-Haskell-code for GTKLabel
            currentNode = label;  //keep track of current Node
        }
    }

    public static void parseGTKEntry(TextField n){
        //extract corresponding attributes of JavaFX TextField
        var width = n.getLayoutBounds().getWidth();
        var height = n.getLayoutBounds().getHeight();
        var layoutX = n.getLayoutX();
        var layoutY = n.getLayoutY();
        var text = ((TextField) n).getText();
        var javafxAlignment = ((TextField) n).getAlignment();
        var haskellAlignment = Entry.getHaskellAlignment(javafxAlignment);
        var placeholder = ((TextField) n).getPromptText();
        var entry = new Entry(n.getId(), n.hashCode(),"entry", text, layoutX, layoutY, width, height, haskellAlignment, placeholder);

        GUIWidgets.add(entry); //add GTKEntry to control (widget) list
        appendTextToFile("-- Entry \n  " + entry.gtkHsCode()); //generate gi-gtk-Haskell-code for GTKEntry
        currentNode = entry; //keep track of current Node
    }

    public static void parseGTKCheckButton(CheckBox n){
        //extract corresponding attributes of JavaFX CheckBox
        var text = ((CheckBox) n).getText();
        var layoutX = n.getLayoutX();
        var layoutY = n.getLayoutY();
        var checkButton = new CheckButton(n.getId(), n.hashCode(), "checkButton", layoutX, layoutY, text);

        GUIWidgets.add(checkButton); //add GTKCheckButton to control (widget) list
        appendTextToFile("-- CheckButton \n  " + checkButton.gtkHsCode()); //generate gi-gtk-Haskell-code for GTKCheckButton
        currentNode = checkButton; //keep track of current Node
    }

    public static void parseGTKRadioButton(javafx.scene.control.RadioButton n){
        //extract corresponding attributes of JavaFX RadioButton
        var layoutX = n.getLayoutX();
        var layoutY = n.getLayoutY();
        var text = ((javafx.scene.control.RadioButton) n).getText();

        var radioButton = new RadioButton(n.getId(), n.hashCode(), "radioButton", layoutX, layoutY, text);

        if (((javafx.scene.control.RadioButton) n).getToggleGroup() != null) {
            var toggleHash = ((javafx.scene.control.RadioButton) n).getToggleGroup().hashCode();
            var buttons = new ArrayList<GTKWidget>();
            var toggleGroup = new ToggleGroup(toggleHash, buttons);
            var rightToggleGroup = toggleGroup.getToggleGroup(toggleGroups);
            rightToggleGroup.addButtonToToggleGroup(radioButton);
            toggleGroups.add(rightToggleGroup);
        }

        GUIWidgets.add(radioButton); //add GTKRadioButton to control (widget) list
        appendTextToFile("-- RadioButton \n  " + radioButton.gtkHsCode()); //generate gi-gtk-Haskell-code for GTKRadioButton
        currentNode = radioButton; //keep track of current Node
    }


    public static void parseGTKGrid(GridPane n){
        //extract corresponding attributes of JavaFX GridPane
        var layoutX = n.getLayoutX();
        var layoutY = n.getLayoutY();
        var columnSpacing = ((GridPane) n).getHgap();
        var rowSpacing = ((GridPane) n).getVgap();
        var gridP = new Grid(n.getId(), n.hashCode(),"grid", layoutX, layoutY, columnSpacing, rowSpacing);

        GUIContainers.add(gridP); //add GTKGrid to container list
        appendTextToFile("-- Grid \n  " + gridP.gtkHsCode()); //generate gi-gtk-Haskell-code for GTKGridpane
        currentNode = gridP; //keep track of current Node
    }


    public static void parseGTKVBox(VBox n){
        //extract corresponding attributes of JavaFX VBox
        var layoutX = n.getLayoutX();
        var layoutY = n.getLayoutY();
        var vSpacing = (int)((VBox) n).getSpacing();
        Box vBox = new Box(n.getId(), n.hashCode(),"vBox", layoutX, layoutY, vSpacing, Orientation.OrientationVertical);

        GUIContainers.add(vBox); //add GTKBox to container list
        appendTextToFile("-- vBox \n  " + vBox.gtkHsCode()); //generate gi-gtk-Haskell-code for GTKBox
        currentNode = vBox; //keep track of current Node
    }

    public static void parseGTKHBox(HBox n){
        //extract corresponding attributes of JavaFX HBox
        var layoutX = n.getLayoutX();
        var layoutY = n.getLayoutY();
        var hSpacing = (int)((HBox) n).getSpacing();
        Box hBox = new Box(n.getId(), n.hashCode(),"hBox", layoutX, layoutY, hSpacing, Orientation.OrientationHorizontal);

        GUIContainers.add(hBox);  //add GTKBox to container list
        appendTextToFile("-- hBox \n  " + hBox.gtkHsCode()); //generate gi-gtk-Haskell-code for GTKBox
        currentNode = hBox; //keep track of current Node
    }

    public static void parseGTKNotebook(TabPane n){
        //extract corresponding attributes of JavaFX TabPane
        var layoutX = n.getLayoutX();
        var layoutY = n.getLayoutY();
        var width = (int)((TabPane) n).getWidth();
        var height = (int)((TabPane) n).getHeight();

        //Extract Notebook tabs (Labels) and directly insert to haskell file
        var tabs = ((TabPane) n).getTabs();
        ArrayList<Label> notebookTabs = new ArrayList<>();
        for(Tab tab: tabs){
            var labelText = tab.getText(); //label text
            Label tabLabel = new Label(null ,labelText.hashCode(), "tabLabel", labelText, 0 ,0);
            notebookTabs.add(tabLabel);
            appendTextToFile("--Label \n  " + tabLabel.gtkHsCode());
            GUIWidgets.add(tabLabel); //toevoegen aan globale lijst
        }

        //Create Notebook
        Notebook tabPane = new Notebook(n.getId(), n.hashCode(),"noteBook", layoutX, layoutY, width, height, notebookTabs);
        GUIContainers.add(tabPane);
        appendTextToFile("--Notebook \n  " + tabPane.gtkHsCode());
        currentNode = tabPane;

        //Create Notebook Relation -> exception where relations are already made in the parsing step
        for(int i = 0; i < tabs.size(); i++){
            var tabTopLevelContainer = tabs.get(i).getContent(); //top level elements in a tab -> mostly AnchorPane
            var topLevelContainerHash = String.valueOf(tabTopLevelContainer.hashCode()); //first set hash of child as childName for relation
            var notebookRel = new NotebookRelation(tabPane.getNotebookName(), topLevelContainerHash, notebookTabs.get(i).getName()); //create relation
            notebookRelations.add(notebookRel); //add relation
        }
    }


    public static void parseGTKLinkButton(Hyperlink n){
        //extract corresponding attributes of JavaFX Hyperlink
        var layoutX = n.getLayoutX();
        var layoutY = n.getLayoutY();
        var text = ((Hyperlink) n).getText();
        var visited = ((Hyperlink) n).isVisited();
        var linkButton = new LinkButton(n.getId(), n.hashCode(), "linkButton", layoutX, layoutY, text, visited);

        GUIWidgets.add(linkButton);  //add GTKLinkButton to control list
        appendTextToFile("-- LinkButton \n  " + linkButton.gtkHsCode()); //generate gi-gtk-Haskell-code for GTKLinkButton
        currentNode = linkButton; //keep track of current Node
    }

    public static void parseGTKComboBox(ComboBox<?> n){
        //extract corresponding attributes of JavaFX Hyperlink
        var layoutX = n.getLayoutX();
        var layoutY = n.getLayoutY();
        var entry = ((ComboBox<?>) n).isEditable();
        var items = ((ComboBox<?>) n).getItems();
        var width = ((ComboBox<?>) n).getWidth();
        var height = ((ComboBox<?>) n).getHeight();
        var comboBoxText = new ComboBoxText(n.getId(), n.hashCode(), "comboBoxText", layoutX, layoutY, entry, items, width, height);

        GUIWidgets.add(comboBoxText); //add GTKComboBox to control list
        appendTextToFile("-- ComboBoxText \n  " + comboBoxText.gtkHsCode()); //generate gi-gtk-Haskell-code for GTKComboBOx
        currentNode = comboBoxText;  //keep track of current Node
    }

    public static void generateHsCodeMiddlePart(){
        dump(stage.getScene().getRoot());
    }

    public static Stage getStage() {
        return stage;
    }

    public static void setStage(Stage stage) {
        GtkHaskellCode.stage = stage;
    }

    /** Debugging routine to dump the scene graph. */
    public static void dump(Node n) {
        dump(n, 0);
    }

    private static void dump(Node n, int depth) {
        for (int i = 0; i < depth; i++) System.out.print("  ");

        System.out.println(n);

        //CREATE Data structures
        if(n instanceof javafx.scene.control.Button){
            parseGTKButton((javafx.scene.control.Button) n);
        }else if (n instanceof AnchorPane){
            parseGTKLayout((AnchorPane) n);
        } else if (n instanceof javafx.scene.control.Label){
            parseGTKLabel((javafx.scene.control.Label) n);
        } else if (n instanceof TextField){
            parseGTKEntry((TextField) n);
        } else if(n instanceof CheckBox) {
            parseGTKCheckButton((CheckBox) n);
        } else if (n instanceof javafx.scene.control.RadioButton){
            parseGTKRadioButton((javafx.scene.control.RadioButton) n);
        } else if (n instanceof GridPane){
            parseGTKGrid((GridPane) n);
        } else if (n instanceof VBox){
           parseGTKVBox((VBox) n);
        } else if (n instanceof HBox){
            parseGTKHBox((HBox) n);
        } else if (n instanceof TabPane){
            parseGTKNotebook((TabPane) n);
        } else if (n instanceof Hyperlink){
            parseGTKLinkButton((Hyperlink) n);
        } else if (n instanceof ComboBox<?>){
            parseGTKComboBox((ComboBox<?>) n);
        }

        //CREATE RELATIONS
        if(n.getParent() instanceof AnchorPane){ //if parent is container like AnchorPane -> create relation
            makeGTKLayoutRelation(n);
        } else if(n.getParent() instanceof GridPane){ //if parent is container like GridPane -> create relation
           makeGTKGridRelation(n);
        } else if(n.getParent() instanceof HBox || n.getParent() instanceof VBox){ //if parent is container like H/VBox -> create relation
            makeGTKBoxRelation(n);
        }

        if (n instanceof Parent) {
            for (Node c : ((Parent) n).getChildrenUnmodifiable()) {
                dump(c, depth + 1);
            }
        }
    }

    public static void makeGTKLayoutRelation(Node n){
        //extract corresponding attributes for relation
        String APParentName = getParentName(n.getParent().hashCode());
        int x;
        int y;
        x  = (int)currentNode.getLayoutX();
        y  = (int)currentNode.getLayoutY();
        LayoutRelation layoutRel = new LayoutRelation(APParentName, currentNode.getName(), x, y);

        relations.add(layoutRel);
    }

    public static void makeGTKGridRelation(Node n){
        //extract corresponding attributes for relation
        String gridPaneParentName = getParentName(n.getParent().hashCode());
        Integer row = GridPane.getRowIndex(n);
        Integer column = GridPane.getColumnIndex(n);
        if (row == null) row= 0;
        if (column == null) column= 0;
        GridRelation gridRel = new GridRelation(gridPaneParentName, currentNode.getName(), row, column);

        relations.add(gridRel);
    }

    public static void makeGTKBoxRelation(Node n){
        String boxParentName = getParentName(n.getParent().hashCode());
        BoxRelation boxRel = new BoxRelation(boxParentName, currentNode.getName(), 0);
        relations.add(boxRel);
    }

    public static String getParentName(Integer parentID){
        String parentName;
        for (GTKWidget container: GUIContainers){
            if(container.getId_hash().equals(parentID)){
                parentName = container.getName();
                return parentName;
            }
        }
        return "";
    }

    public static boolean checkLabelInTab(Node n){
        var parent = n.getParent();  //kijken of de label in kwestie uberhaupt een parent heeft
        if(parent == null){
            return false;
        }

        if(n.getParent().getClass().toString().contains("TabPaneSkin")){  //indien de label tot een tabPane behoort -> werkelijke parent in de Tree = TabPaneSkin inner class
            return true;
        }else{
            return false;
        }
    }
}
