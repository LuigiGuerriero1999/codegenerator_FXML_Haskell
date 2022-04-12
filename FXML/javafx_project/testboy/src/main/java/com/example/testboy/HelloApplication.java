package com.example.testboy;

import com.example.testboy.structures.*;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.stage.Stage;

import java.io.*;
import java.util.ArrayList;

public class HelloApplication extends Application {
    @Override
    public void start(Stage primaryStage) throws IOException {
        relations = new ArrayList<Relation>();
        GUIelements = new ArrayList<GTKWidget>();
        //stage = FXMLLoader.load(getClass().getResource("simpeleAP_met_button_goed.fxml"));    // Button voorbeeld
        stage = FXMLLoader.load(getClass().getResource("simpeleAP_met_label.fxml"));              // Label voorbeeld
        //stage = FXMLLoader.load(getClass().getResource("simpeleAP_met_entry.fxml"));              // Entry voorbeeld
        //stage = FXMLLoader.load(getClass().getResource("gui_literatuur_metstage.fxml"));
        stage.show();

        dump(stage.getScene().getRoot());

        //generateButtonCode();
        generateLabelCode();
        //generateEntryCode();
    }

    private Button testButton;
    private Label testLabel;
    private Entry testEntry;
    private Stage stage;
    private Layout layout;
    private String username = System.getProperty("user.name"); //voor gebruikersnaam voor padnaam waar .hs file opgeslagen moet worden
    private String path = "/home/"+username+"/Documents/Masterproef/FXML/javafx_project/testboy/src/main/java/com/example/testboy/gi-gtk-generated.hs";

    private ArrayList<Relation> relations ;
    private ArrayList<GTKWidget> GUIelements;

    public String createImports(){
        String gtkHsCode =  "{-# LANGUAGE OverloadedStrings #-}\n" +
                            "{-# LANGUAGE OverloadedLabels  #-}\n" +
                            "{-# LANGUAGE ExtendedDefaultRules #-}\n"+
                            "\n"+
                            "module Main (Main.main) where\n" +
                            "import qualified GI.Gtk as Gtk\n" +
                            "import GI.Gtk.Enums (WindowType(..), Orientation(..))\n" +
                            "import GI.Gtk (Adjustment(Adjustment))\n" +
                            "import Data.GI.Base ( AttrOp((:=)) )\n" +
                            "\n"+
                            "main :: IO ()\n" +
                            "main = do\n" +
                            "  Gtk.init Nothing\n  " ;
        return gtkHsCode;
    }

    public String createTopLevelWindow(){
        String stageResizableBool = String.valueOf(stage.isResizable()).substring(0, 1).toUpperCase() + String.valueOf(stage.isResizable()).substring(1);
        String createTopLevelWindow = "window <- Gtk.windowNew WindowTypeToplevel\n  ";
        String setWindowProperties =  "Gtk.set window [Gtk.windowTitle := "+"\""+stage.getTitle()+"\""+", Gtk.windowResizable := "+stageResizableBool+", Gtk.windowDefaultWidth := "+(int)stage.getWidth()+", Gtk.windowDefaultHeight := "+(int)stage.getHeight()+"]\n  ";
        String gtkHsCode = createTopLevelWindow + setWindowProperties;
        return gtkHsCode;
    }

    public String endMainProgram(){
        String onWidgetDestroy = "Gtk.onWidgetDestroy window Gtk.mainQuit\n  ";
        String widgetShowAll = "Gtk.widgetShowAll window\n  ";
        String main = "Gtk.main\n  ";
        String gtkHsCode = onWidgetDestroy+widgetShowAll+main;
        return gtkHsCode;
    }

    public String generateRelations(){
        String relationsGtkCode = "";
        for(Relation r : this.relations){
            relationsGtkCode = r.generateGtkHsCode(GUIelements);
        }
        return relationsGtkCode;
    }

    public String bindTopLevelElementToWindow(){
        var topLevelElement = stage.getScene().getRoot();
        var hashIdTopElement = topLevelElement.hashCode();
        String topLevelBinding = "";
        for(GTKWidget q : GUIelements){
            if(q.getId_hash().equals(hashIdTopElement)){
                topLevelBinding = "Gtk.setContainerChild window "+q.getName()+"\n  ";
            }
        }
        return topLevelBinding;
    }

    public static void main(String[] args) {
        launch();
    }

    /** Debugging routine to dump the scene graph. */
    public void dump(Node n) {
        dump(n, 0);
    }

    private void dump(Node n, int depth) {
        for (int i = 0; i < depth; i++) System.out.print("  ");
        // getTypeSelector = returns type Widget
        // isResizable = returns resizable Boolean
        // getLayoutBounds = returns [minX:0.0, minY:0.0, minZ:0.0, width:600.0, height:400.0, depth:0.0, maxX:600.0, maxY:400.0, maxZ:0.0]

        /*System.out.print(n.getTypeSelector());
        System.out.print(" Resizable:"+n.isResizable());
        System.out.print(" Width:"+n.getLayoutBounds().getWidth());
        System.out.print(" Height:"+n.getLayoutBounds().getHeight());
        System.out.print(" LayoutX:"+n.getLayoutX());
        System.out.print(" LayoutY:"+n.getLayoutY());
        System.out.println();*/


        System.out.println(n);
        if(n instanceof javafx.scene.control.Button){
            //System.out.println(n);
            var width = n.getLayoutBounds().getWidth();
            var height = n.getLayoutBounds().getHeight();
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var label = ((javafx.scene.control.Button) n).getText();
            var button = new Button(n.getId(), n.hashCode(),"buttonShowText", label, layoutX, layoutY, width, height);
            this.testButton =  button;
            GUIelements.add(button);

            if(n.getParent() != null){
                //System.out.println("Parent is: "+n.getParent());
            }
        }else if (n instanceof AnchorPane){
            var width = n.getLayoutBounds().getWidth();
            var height = n.getLayoutBounds().getHeight();
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            layout = new Layout(n.getId(), n.hashCode(),"layout", layoutX, layoutY,width ,height);
            GUIelements.add(layout);

            for(Node elementInLayout : ((AnchorPane) n).getChildren()) {
                LayoutRelation relation = new LayoutRelation(n.hashCode(), elementInLayout.hashCode());
                relations.add(relation);
            }
        } else if (n instanceof javafx.scene.control.Label){
            var width = n.getLayoutBounds().getWidth();
            var height = n.getLayoutBounds().getHeight();
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var text = ((javafx.scene.control.Label) n).getText();
            var label = new Label(n.getId(), n.hashCode(),"labelTest", text, layoutX, layoutY, width, height);
            this.testLabel = label;
            GUIelements.add(label);
        } else if (n instanceof TextField){
            var width = n.getLayoutBounds().getWidth();
            var height = n.getLayoutBounds().getHeight();
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var text = ((javafx.scene.control.TextField) n).getText();
            var entry = new Entry(n.getId(), n.hashCode(),"entryTest", text, layoutX, layoutY, width, height);
            this.testEntry = entry;
            GUIelements.add(entry);
        } else if (n instanceof GridPane){
            for(Node elementInGrid : ((GridPane) n).getChildren()) {
                Integer row = GridPane.getRowIndex(elementInGrid);
                Integer column = GridPane.getColumnIndex(elementInGrid);
                if (row == null) row= 0;
                if (column == null) column= 0;
                GridRelation relation = new GridRelation(n.hashCode(), elementInGrid.hashCode(), row, column);
                relations.add(relation);
            }
        }

        if (n instanceof Parent) {
            for (Node c : ((Parent) n).getChildrenUnmodifiable()) {
                dump(c, depth + 1);
            }
        }
    }

    private void generateButtonCode(){
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(path), "utf-8"))) {
            writer.write(
                        createImports() +
                            createTopLevelWindow()+
                            "\n  "+
                            testButton.gtkHsCode() +
                            "\n  " +
                            layout.gtkHsCode() +
                            generateRelations() +
                            "\n  "+
                            bindTopLevelElementToWindow() +
                            "\n  " +
                            endMainProgram()
                        );
        }catch(IOException e){
            System.out.println(e.toString());
        }
    }

    private void generateLabelCode(){
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(path), "utf-8"))) {
            writer.write(
                    createImports() +
                            createTopLevelWindow()+
                            "\n  "+
                            testLabel.gtkHsCode() +
                            "\n  " +
                            layout.gtkHsCode() +
                            generateRelations() +
                            "\n  "+
                            bindTopLevelElementToWindow() +
                            "\n  " +
                            endMainProgram()
            );
        }catch(IOException e){
            System.out.println(e.toString());
        }
    }

    private void generateEntryCode(){
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream(path), "utf-8"))) {
            writer.write(
                    createImports() +
                            createTopLevelWindow()+
                            "\n  "+
                            testEntry.gtkHsCode() +
                            "\n  " +
                            layout.gtkHsCode() +
                            generateRelations() +
                            "\n  "+
                            bindTopLevelElementToWindow() +
                            "\n  " +
                            endMainProgram()
            );
        }catch(IOException e){
            System.out.println(e.toString());
        }
    }
}