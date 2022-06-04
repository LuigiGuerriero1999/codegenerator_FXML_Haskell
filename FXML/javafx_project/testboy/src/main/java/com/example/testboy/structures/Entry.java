package com.example.testboy.structures;

import com.example.testboy.HelloApplication;
import com.example.testboy.StringFormat;
import javafx.geometry.Pos;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class Entry extends GTKWidget{
    private String text;
    private String entryName;

    private double width;
    private double height;

    private double alignment;

    private String placeholder;

    public Entry(String text, double layoutX, double layoutY, double width, double height, double alignment, String placeholder) {
        this.text = text;
        setLayoutX(layoutX);
        setLayoutY(layoutY);
        this.width = width;
        this.height = height;
        this.alignment = alignment;
        this.placeholder = placeholder;
    }

    public Entry(String id, Integer id_hash, String name, String text, double layoutX, double layoutY, double width, double height, double alignment, String placeholder) {
        super(id, id_hash, makeName(id,id_hash,name)+"Container", layoutX, layoutY);
        this.text = text;
        this.width = width;
        this.height = height;
        this.alignment = alignment;
        this.placeholder = placeholder;
        this.entryName = makeName(id,id_hash,name);
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getEntryName() {
        return entryName;
    }

    public void setEntryName(String entryName) {
        this.entryName = entryName;
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

    public double getAlignment() {
        return alignment;
    }

    public void setAlignment(double alignment) {
        this.alignment = alignment;
    }

    public String getPlaceholder() {
        return placeholder;
    }

    public void setPlaceholder(String placeholder) {
        this.placeholder = placeholder;
    }

    public static double getHaskellAlignment(Pos javafxAlignment){
        if (javafxAlignment == Pos.BASELINE_CENTER ||
            javafxAlignment == Pos.BOTTOM_CENTER ||
            javafxAlignment == Pos.CENTER ||
            javafxAlignment == Pos.TOP_CENTER) {
            return 0.5;
        } else if (javafxAlignment == Pos.BASELINE_LEFT ||
                   javafxAlignment == Pos.BOTTOM_LEFT ||
                   javafxAlignment == Pos.CENTER_LEFT ||
                   javafxAlignment == Pos.TOP_LEFT) {
            return 0;
        } else {
            return 1;
        }
    }

    @Override
    public String gtkHsCode() {
        StringBuilder template = new StringBuilder();
        template.append("${ENTRYNAME} <- Gtk.entryNew\n  ");
        if (!Objects.equals(text, "")) template.append("Gtk.entrySetText ${ENTRYNAME} \"${TEXT}\" \n  ");
        if (!Objects.equals(placeholder, "")) template.append("Gtk.entrySetPlaceholderText ${ENTRYNAME} (Just \"${PLACEHOLDER}\")\n  ");
        if (!Objects.equals(alignment, 0)) template.append("Gtk.entrySetAlignment ${ENTRYNAME} ${ALLIGNMENT} \n  ");
        template.append("${ENTRYCONTAINERBOXNAME} <- Gtk.boxNew OrientationHorizontal 1\n  ");
        template.append("Gtk.set ${ENTRYCONTAINERBOXNAME} [Gtk.widgetWidthRequest :=${WIDTH},  Gtk.widgetHeightRequest :=${HEIGHT}]\n  ");
        template.append("Gtk.boxPackStart ${ENTRYCONTAINERBOXNAME} ${ENTRYNAME} True True 0\n   \n  ");

        Map<String, Object> toInsert = new HashMap<String, Object>();
        toInsert.put("ENTRYCONTAINERBOXNAME", super.getName());
        toInsert.put("ENTRYNAME", entryName);
        toInsert.put("WIDTH", (int)width);
        toInsert.put("HEIGHT", (int)height);
        toInsert.put("ALLIGNMENT", alignment);
        toInsert.put("PLACEHOLDER", placeholder);
        toInsert.put("TEXT", text);

        return StringFormat.format(template.toString(), toInsert);
    }
}
