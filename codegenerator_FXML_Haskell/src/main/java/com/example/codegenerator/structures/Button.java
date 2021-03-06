package com.example.codegenerator.structures;

import main.java.com.example.codegenerator.StringFormat;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class Button extends GTKWidget {
    private String label;
    private String buttonName;

    private double width;
    private double height;

    public Button(String label, double layoutX, double layoutY, double width, double height) {
        this.label = label;
        setLayoutX(layoutX);
        setLayoutY(layoutY);
        this.width = width;
        this.height = height;
    }

    public Button(String id, Integer id_hash, String name, String label, double layoutX, double layoutY, double width, double height) {
        super(id, id_hash, makeName(id,id_hash,name)+"Container", layoutX, layoutY);
        this.label = label;
        this.width = width;
        this.height = height;
        this.buttonName = makeName(id,id_hash,name);
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getButtonName() {
        return buttonName;
    }

    public void setButtonName(String buttonName) {
        this.buttonName = buttonName;
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

    @Override
    public String gtkHsCode(){
        StringBuilder template = new StringBuilder();
        if (!Objects.equals(label, "")) template.append("${BUTTONNAME} <- Gtk.buttonNewWithLabel \"${LABEL}\"\n  ");
        else template.append("${BUTTONNAME} <- Gtk.buttonNew \n  ");
        template.append("${BUTTONCONTAINERBOXNAME} <- Gtk.boxNew OrientationHorizontal 1\n  ");
        template.append("Gtk.set ${BUTTONCONTAINERBOXNAME} [Gtk.widgetWidthRequest :=${WIDTH},  Gtk.widgetHeightRequest :=${HEIGHT}]\n  ");
        template.append("Gtk.boxPackStart ${BUTTONCONTAINERBOXNAME} ${BUTTONNAME} True True 0\n   \n  ");

        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("BUTTONCONTAINERBOXNAME", super.getName());
        toInsert.put("WIDTH", (int)width);
        toInsert.put("HEIGHT", (int)height);
        toInsert.put("BUTTONNAME", buttonName);
        toInsert.put("LABEL", label);

        return StringFormat.format(template.toString(),toInsert);
    }
}
