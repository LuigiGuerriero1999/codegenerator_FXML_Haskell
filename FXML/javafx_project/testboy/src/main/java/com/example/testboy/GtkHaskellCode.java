package com.example.testboy;

import com.example.testboy.orientation.Orientation;
import com.example.testboy.relations.*;
import com.example.testboy.structures.*;
import com.example.testboy.structures.Button;
import com.example.testboy.structures.Label;
import com.example.testboy.structures.RadioButton;
import com.example.testboy.togglegroup.ToggleGroup;
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

    private static final String username = System.getProperty("user.name"); //voor gebruikersnaam voor padnaam waar .hs file opgeslagen moet worden
    private static final String path = "/home/"+username+"/Documents/Masterproef/FXML/javafx_project/testboy/src/main/java/com/example/testboy/gi-gtk-generated.hs";

    private static ArrayList<Relation> relations ;
    private static ArrayList<GTKWidget> GUIWidgets;
    private static ArrayList<GTKWidget> GUIContainers;
    private static ArrayList<ToggleGroup> toggleGroups;
    private static ArrayList<NotebookRelation> notebookRelations;


    private static GTKWidget currentNode; //voorlopige node tijdens het doorlopen van FXML

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
        return relationComment + relationsGtkCode;
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
        String relationComment = "--Notebook relations \n  ";
        StringBuilder relationsGtkCode = new StringBuilder();
        for(NotebookRelation r : notebookRelations){
            r.setContainerList(GUIContainers);
            r.correctChildName();
            relationsGtkCode.append(r.generateGtkHsCode());
        }
        return relationComment + relationsGtkCode;
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

    /** Debugging routine to dump the scene graph. */
    public static void dump(Node n) {
        dump(n, 0);
    }

    private static void dump(Node n, int depth) {
        for (int i = 0; i < depth; i++) System.out.print("  ");

        System.out.println(n);
        if(n instanceof javafx.scene.control.Button){
            var width = n.getLayoutBounds().getWidth();
            var height = n.getLayoutBounds().getHeight();
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var label = ((javafx.scene.control.Button) n).getText();
            var button = new Button(n.getId(), n.hashCode(),"button", label, layoutX, layoutY, width, height);
            GUIWidgets.add(button);

            appendTextToFile("-- Button \n  " + button.gtkHsCode());

            currentNode = button;
        }else if (n instanceof AnchorPane){
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var layout = new Layout(n.getId(), n.hashCode(),"layout", layoutX, layoutY);
            GUIContainers.add(layout);

            appendTextToFile("-- Layout \n  " + layout.gtkHsCode());

            currentNode = layout;
        } else if (n instanceof javafx.scene.control.Label){
            if(!checkLabelInTab(n)){  //indien Label niet onder deel is van een Tab -> uitzondering, niet hier aanmaken, maar met de tabPane samen
                var layoutX = n.getLayoutX();
                var layoutY = n.getLayoutY();
                var text = ((javafx.scene.control.Label) n).getText();
                var label = new Label(n.getId(), n.hashCode(),"label", text, layoutX, layoutY);
                GUIWidgets.add(label);

                appendTextToFile("--Label \n  " + label.gtkHsCode());

                currentNode = label;
            }
        } else if (n instanceof TextField){
            var width = n.getLayoutBounds().getWidth();
            var height = n.getLayoutBounds().getHeight();
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var text = ((TextField) n).getText();
            var javafxAlignment = ((TextField) n).getAlignment();
            var haskellAlignment = Entry.getHaskellAlignment(javafxAlignment);
            var placeholder = ((TextField) n).getPromptText();
            var entry = new Entry(n.getId(), n.hashCode(),"entry", text, layoutX, layoutY, width, height, haskellAlignment, placeholder);
            GUIWidgets.add(entry);

            appendTextToFile("-- Entry \n  " + entry.gtkHsCode());

            currentNode = entry;
        } else if(n instanceof CheckBox) {
            var text = ((CheckBox) n).getText();
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var checkButton = new CheckButton(n.getId(), n.hashCode(), "checkButton", layoutX, layoutY, text);
            GUIWidgets.add(checkButton);

            appendTextToFile("-- CheckButton \n  " + checkButton.gtkHsCode());

            currentNode = checkButton;
        } else if (n instanceof javafx.scene.control.RadioButton){
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

            GUIWidgets.add(radioButton);

            appendTextToFile("-- RadioButton \n  " + radioButton.gtkHsCode());

            currentNode = radioButton;
        } else if (n instanceof GridPane){
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var columnSpacing = ((GridPane) n).getHgap();
            var rowSpacing = ((GridPane) n).getVgap();
            var gridP = new Grid(n.getId(), n.hashCode(),"grid", layoutX, layoutY, columnSpacing, rowSpacing);
            GUIContainers.add(gridP);

            appendTextToFile("-- Grid \n  " + gridP.gtkHsCode());

            currentNode = gridP;
        } else if (n instanceof VBox){
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var vSpacing = (int)((VBox) n).getSpacing();
            Box vBox = new Box(n.getId(), n.hashCode(),"vBox", layoutX, layoutY, vSpacing, Orientation.OrientationVertical);
            GUIContainers.add(vBox);

            appendTextToFile("-- vBox \n  " + vBox.gtkHsCode());

            currentNode = vBox;
        } else if (n instanceof HBox){
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var hSpacing = (int)((HBox) n).getSpacing();
            Box hBox = new Box(n.getId(), n.hashCode(),"hBox", layoutX, layoutY, hSpacing, Orientation.OrientationHorizontal);
            GUIContainers.add(hBox);

            appendTextToFile("-- hBox \n  " + hBox.gtkHsCode());

            currentNode = hBox;
        } else if (n instanceof TabPane){
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var width = (int)((TabPane) n).getWidth();
            var height = (int)((TabPane) n).getHeight();

            //Notebook tabs extracten en in file zetten
            var tabs = ((TabPane) n).getTabs();
            ArrayList<Label> notebookTabs = new ArrayList<>();
            for(Tab tab: tabs){
                var labelText = tab.getText(); //label text
                Label tabLabel = new Label(null ,labelText.hashCode(), "tabLabel", labelText, 0 ,0);
                notebookTabs.add(tabLabel);
                appendTextToFile("--Label \n  " + tabLabel.gtkHsCode());
                GUIWidgets.add(tabLabel); //toevoegen aan globale lijst
            }

            //Notebook aanmaken
            Notebook tabPane = new Notebook(n.getId(), n.hashCode(),"noteBook", layoutX, layoutY, width, height, notebookTabs);
            GUIContainers.add(tabPane);
            appendTextToFile("--Notebook \n  " + tabPane.gtkHsCode());
            currentNode = tabPane;

            //Notebook relaties aanmaken
            for(int i = 0; i < tabs.size(); i++){
                var tabTopLevelContainer = tabs.get(i).getContent(); //top level elementen in een tab -> meestal AnchorPane
                var topLevelContainerHash = String.valueOf(tabTopLevelContainer.hashCode()); //we gaan eerst de hash van de child als childName setten in de relatie
                var notebookRel = new NotebookRelation(tabPane.getNotebookName(), topLevelContainerHash, notebookTabs.get(i).getName());
                notebookRelations.add(notebookRel);
            }
        } else if (n instanceof Hyperlink){
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var text = ((Hyperlink) n).getText();
            var visited = ((Hyperlink) n).isVisited();
            var linkButton = new LinkButton(n.getId(), n.hashCode(), "linkButton", layoutX, layoutY, text, visited);
            GUIWidgets.add(linkButton);

            appendTextToFile("-- LinkButton \n  " + linkButton.gtkHsCode());

            currentNode = linkButton;
        } else if (n instanceof ComboBox<?>){
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var entry = ((ComboBox<?>) n).isEditable();
            var items = ((ComboBox<?>) n).getItems();
            var width = ((ComboBox<?>) n).getWidth();
            var height = ((ComboBox<?>) n).getHeight();
            var comboBoxText = new ComboBoxText(n.getId(), n.hashCode(), "comboBoxText", layoutX, layoutY, entry, items, width, height);
            GUIWidgets.add(comboBoxText);

            appendTextToFile("-- ComboBoxText \n  " + comboBoxText.gtkHsCode());

            currentNode = comboBoxText;
        }

        //RELATIES AANMAKEN
        if(n.getParent() instanceof AnchorPane){ //indien parent een container is zoals AnchorPane -> relatie aanmaken
            String APParentName = getParentName(n.getParent().hashCode());
            int x;
            int y;

            x  = (int)currentNode.getLayoutX();
            y  = (int)currentNode.getLayoutY();

            LayoutRelation layoutRel = new LayoutRelation(APParentName, currentNode.getName(), x, y);
            relations.add(layoutRel);
        } else if(n.getParent() instanceof GridPane){
            String gridPaneParentName = getParentName(n.getParent().hashCode());
            Integer row = GridPane.getRowIndex(n);
            Integer column = GridPane.getColumnIndex(n);
            if (row == null) row= 0;
            if (column == null) column= 0;
            GridRelation gridRel = new GridRelation(gridPaneParentName, currentNode.getName(), row, column);
            relations.add(gridRel);
        } else if(n.getParent() instanceof HBox || n.getParent() instanceof VBox){
            String boxParentName = getParentName(n.getParent().hashCode());
            BoxRelation boxRel = new BoxRelation(boxParentName, currentNode.getName(), 0);
            relations.add(boxRel);
        }

        if (n instanceof Parent) {
            for (Node c : ((Parent) n).getChildrenUnmodifiable()) {
                dump(c, depth + 1);
            }
        }
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
