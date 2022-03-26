package com.example.testboy;

import com.example.testboy.structures.Button;
import com.example.testboy.structures.Layout;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class HelloApplication extends Application {
    @Override
    public void start(Stage primaryStage) throws IOException {
        stage = FXMLLoader.load(getClass().getResource("simpeleAP_met_button_goed.fxml"));
        stage.show();


        dump(stage.getScene().getRoot());

        generateCode();
    }

    private Button testButton;
    private Stage stage;
    private Layout layout;

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

        if(n instanceof javafx.scene.control.Button){
            System.out.println(n);
            var width = n.getLayoutBounds().getWidth();
            var height = n.getLayoutBounds().getHeight();
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            var label = ((javafx.scene.control.Button) n).getText();
            var button = new Button("buttonShowText", label, layoutX, layoutY, width, height);
            this.testButton =  button;
        }else if (n instanceof AnchorPane){
            var width = n.getLayoutBounds().getWidth();
            var height = n.getLayoutBounds().getHeight();
            var layoutX = n.getLayoutX();
            var layoutY = n.getLayoutY();
            layout = new Layout("layout", layoutX, layoutY,width ,height);
            System.out.println("ANCHORPANEBOYKE"+layoutX);
        }

        if (n instanceof Parent) {
            for (Node c : ((Parent) n).getChildrenUnmodifiable()) {
                dump(c, depth + 1);
            }
        }
    }

    private void generateCode(){
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(
                new FileOutputStream("/home/luigi/Documents/Masterproef/FXML/javafx_project/testboy/src/main/java/com/example/testboy/gi-gtk-generated.hs"), "utf-8"))) {
            writer.write(
                    "{-# LANGUAGE OverloadedStrings #-}\n" +
                            "{-# LANGUAGE OverloadedLabels  #-}\n" +
                            "{-# LANGUAGE ExtendedDefaultRules #-}\n"+
                            "\n"+
                            "\n"+
                            "\n"+
                            "module Main (Main.main) where\n" +
                            "import qualified GI.Gtk as Gtk\n" +
                            "import GI.Gtk.Enums (WindowType(..), Orientation(..))\n" +
                            "import GI.Gtk (Adjustment(Adjustment))\n" +
                            "import Data.GI.Base ( AttrOp((:=)) )\n" +
                            "\n"+
                            "\n"+
                            "\n"+
                            "main :: IO ()\n" +
                            "main = do\n" +
                            "  Gtk.init Nothing\n  " +
                            createTopLevelWindow()+
                            "\n  "+
                            testButton.gtkHsCode() +
                            "\n  " +
                            layout.gtkHsCode() +
                            "Gtk.layoutPut layoutContainer buttonShowTextContainerBox 103 109\n" +
                            "\n"+
                            "  \n" +
                            "  Gtk.setContainerChild window layoutContainer \n" +
                            "  \n  " +
                            endMainProgram());
        }catch(IOException e){
            System.out.println(e.toString());
        }
    }
}