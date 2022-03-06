package com.example.testboy;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class HelloApplication extends Application {
    @Override
    public void start(Stage stage) throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader(HelloApplication.class.getResource("test2.fxml"));
        Parent root = (Parent)fxmlLoader.load();

        List<Node> allNodes = getAllNodes(root);
        for(Node node : allNodes) {
            System.out.println(node.getClass());
        }




        Scene scene = new Scene(root);
        stage.setTitle("Hello!");
        stage.setScene(scene);
        stage.show();

        dump(stage.getScene().getRoot());
    }

    public static void main(String[] args) {
        launch();
    }

    private List<Node> getAllNodes(Parent container) {
        List<Node> nodes = new ArrayList<Node>();
        for(Node node : container.getChildrenUnmodifiable())
        {
            nodes.add(node);
            if (node instanceof Parent) {
                Parent subContainer = (Parent) node;
                nodes.addAll( getAllNodes(subContainer) );
            }
        }
        return nodes;
    }

    /** Debugging routine to dump the scene graph. */
    public static void dump(Node n) {
        dump(n, 0);
    }

    private static void dump(Node n, int depth) {
        for (int i = 0; i < depth; i++) System.out.print("  ");
        System.out.println(n);
        if (n instanceof Parent) {
            for (Node c : ((Parent) n).getChildrenUnmodifiable()) {
                dump(c, depth + 1);
            }
        }
    }
}