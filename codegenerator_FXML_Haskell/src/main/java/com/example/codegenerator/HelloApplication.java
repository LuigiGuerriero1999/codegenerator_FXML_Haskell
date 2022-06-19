package com.example.codegenerator;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.stage.Stage;

import java.io.*;
import java.nio.file.Paths;
import java.util.Objects;

public class HelloApplication extends Application {
    public static String FXML_path;
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
        stage = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("/ultiemeTest.fxml")));
        }else{
            stage = FXMLLoader.load(Paths.get(FXML_path).toUri().toURL());
        }
        stage.show();

        new GtkHaskellCode(stage);

        GtkHaskellCode.generateHaskellCode();
        GtkHaskellCode.dump(stage.getScene().getRoot());
        GtkHaskellCode.endOfFile();
    }

    public static void setFXML_path(String FXML_filename) {
        //String username = System.getProperty("user.name"); //username of host-system-user to store .hs file
        //HelloApplication.FXML_path = "/home/"+username+"/Documents/GitHub/codegenerator_FXML_Haskell/codegenerator_FXML_Haskell/fxml/"+FXML_filename;
        HelloApplication.FXML_path = FXML_filename;
    }

    public static void main(String[] args) {
        setFXML_path("");
        try{
            if (args[0] == null || args[0].trim().isEmpty()) {
                System.out.println("You need to specify a path!");
                return;
            } else {
                System.out.println(args[0]);
                setFXML_path(args[0]);
            }
        }catch (Exception e){
            //setFXML_path("");
        }
        launch();
    }

}