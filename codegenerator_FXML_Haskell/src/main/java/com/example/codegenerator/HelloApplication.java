package com.example.codegenerator;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.SnapshotParameters;
import javafx.scene.image.WritableImage;
import javafx.stage.Stage;

import java.io.*;
import java.nio.file.Paths;
import java.util.Objects;

public class HelloApplication extends Application {
    public static String FXML_path;
    public static String modus;
    public static Stage stage;
    @Override
    public void start(Stage primaryStage) throws IOException {
        if(FXML_path == ""){
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/simpeleAP_met_button_goed.fxml")));    // Button voorbeeld
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/simpeleAP_met_label.fxml")));              // Label voorbeeld
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/simpeleAP_met_entry.fxml")));              // Entry voorbeeld
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/vbox_met_elementen.fxml")));
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/hbox_met_elementen.fxml")));
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/layout_voorbeeld.fxml")));
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/grid_voorbeeld.fxml")));
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/tabpane.fxml")));
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/simpleTabPane.fxml")));
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/gui_literatuur_metstage_checkButton_radioButton.fxml")));
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/gui_literatuur_metstage_CORRECT.fxml")));
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/combobox_choicebox.fxml")));
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/hyperlinkje.fxml")));
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/gui_literatuur_metstage_CORRECT.fxml")));
        //stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/ultiemeTest.fxml")));
        stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/demo.fxml")));
        }else{
            stage = FXMLLoader.load(Paths.get(FXML_path).toUri().toURL());
        }

        //instantiate codegenerator
        GtkHaskellCode codegenerator = new GtkHaskellCode(stage);

        //necessary to ensure a css layout pass has been run on the scene graph before dumping
        WritableImage image = stage.getScene().getRoot().snapshot(new SnapshotParameters(), null);

        if(modus.equals("-runandgenerate")){
            codegenerator.getStage().show();
        }

        codegenerator.generateHaskellCode();      //generate begin code
        codegenerator.generateHsCodeMiddlePart(); //generate middle part
        codegenerator.endOfFile();                //generate end code
    }

    public static void setFXML_path(String FXML_filename) {
        //String username = System.getProperty("user.name"); //username of host-system-user to store .hs file
        //HelloApplication.FXML_path = "/home/"+username+"/Documents/GitHub/codegenerator_FXML_Haskell/codegenerator_FXML_Haskell/fxml/"+FXML_filename;
        HelloApplication.FXML_path = FXML_filename;
    }

    public static void setModus(String modus) {
        HelloApplication.modus = modus;
    }

    public static void main(String[] args) {
        setFXML_path("");
        setModus("");
        try{
            if (args.length >= 1) {
                System.out.println(args[0]);
                setFXML_path(args[0]);
                if(args.length >= 2) {
                    System.out.println(args[1]);
                    setModus(args[1]);
                }
            } else {
                System.out.println("No FXML-file path specified, selecting default FXML-file");
            }
        }catch (Exception e){
            System.out.println(e.toString());
        }

        System.out.println("FXML_path:" + FXML_path);
        System.out.println("MODUS: "+modus);
        //setModus("-runandgenerate");  //only for testing purposes when working in intelliJ without cmd
        launch();
    }

}