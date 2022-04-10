package com.example.testboy.structures;

import com.example.testboy.HelloApplication;

public class Button extends GTKWidget {

    private String label;
    private String buttonName;

    private double layoutX;
    private double layoutY;

    private double width;
    private double height;

    public Button(String label, double layoutX, double layoutY, double width, double height) {
        this.label = label;
        this.layoutX = layoutX;
        this.layoutY = layoutY;
        this.width = width;
        this.height = height;
    }

    public Button(String id, Integer id_hash, String name, String label, double layoutX, double layoutY, double width, double height) {
        super(id, id_hash, HelloApplication.makeName(id,id_hash,name)+"Container");
        this.label = label;
        this.layoutX = layoutX;
        this.layoutY = layoutY;
        this.width = width;
        this.height = height;
        this.buttonName = HelloApplication.makeName(id,id_hash,name);
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public double getLayoutX() {
        return layoutX;
    }

    public void setLayoutX(double layoutX) {
        this.layoutX = layoutX;
    }

    public double getLayoutY() {
        return layoutY;
    }

    public void setLayoutY(double layoutY) {
        this.layoutY = layoutY;
    }

    public double getWidth() {
        return width;
    }

    public void setWidth(double width) {
        this.width = width;
    }

    public double getHeight() {
        return height;
    }

    public void setHeight(double height) {
        this.height = height;
    }

    public String gtkHsCode(){
        String buttonConstructor = buttonName + " <- Gtk.buttonNew\n  ";
        String setButtonProperties = "Gtk.set "+buttonName+" [Gtk.buttonLabel :="+"\""+label+"\""+"]\n  ";
        String buttonContainerBoxName = super.getName();
        String createButtonContainer = buttonContainerBoxName + " <- Gtk.boxNew OrientationHorizontal 1\n  ";
        String setButtonContainerProperties = "Gtk.set "+buttonContainerBoxName+" [Gtk.widgetWidthRequest := "+(int)width+", Gtk.widgetHeightRequest := "+(int)height+"]\n  ";
        String addButtonToContainer = "Gtk.boxPackStart "+buttonContainerBoxName+" "+buttonName+" True True 0\n   ";
        String buttonGtkHsCode = buttonConstructor + setButtonProperties + createButtonContainer + setButtonContainerProperties + addButtonToContainer;

        return buttonGtkHsCode;
    }
}
