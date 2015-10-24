## Changes after 1.1.10 beta

* Fixed tool editor bug (Mantis #47)
* Fixed tooltip drawing (Mantis #48)
* Fixed viewer issues (Mantis #49)

## PhoA 1.1.10 beta
*[Aug 28, 2005]*

* Add pictures wizard:
  * Using **Extract file icons** option you can now disable icon extraction from picture files (it is the default now), which speeds up file list compilation, especially for large lists.
  * An option for filtering picture files by size.
* File operations wizard:
  * An option for renaming picture files being copied or moved, with using picture property values.
  * An option for copying the language file currently used.
* Browser:
  * **Delete pictures from all groups** (shortcut **Ctrl+Del**) and **Delete files** (shortcut **Shift+Ctrl+Del**) functions.
  * **Thumbnail focus rectangle color** option.
* Viewer:
  * Double-clicking the image acts like pressing **Enter**.
  * Viewport center position is preserved on resize and scale change.
  * With **Fit window to picture size** turned off, the last used window size is remembered. Similarly, with **Center window on the desktop** turned off, the last used window position is restored. Additionally, in multimonitor systems the last used monitor is remembered.
* A possibility of assigning custom icons for picture groups.
* An option for launching an external viewer instead of built-in one, using new **External viewer** tool kind.
* New **-r** command-line option.
* More accurate window handling in multimonitor systems.
* Help:
  * Added **Common tasks** chapter.
  * Added **Acknowledgements** page.
* Other fixes (see the bug tracker for details).

## PhoA 1.1.9
*[Jan 21, 2005]*

* It is now allowed to create a view with no groupings. For example, a view can have a filter expression only.
* Other fixes (see the bug tracker for details).

## PhoA 1.1.8 beta
*[Dec 31, 2004]*

* A long-awaited change picture file feature: **File properties** page of the Picture properties dialog.
* Initial support for **DNG** (Digital Negative) image format from Adobe.
* Failsafe photo album file saving.
* Optimized unlinked picture search and remove routine which allows for significantly faster group and picture deletion.
* Other fixes (see the bug tracker for details).

## PhoA 1.1.7 beta
*[Dec 07, 2004]*

* A possibility for using the fast JPEG decoding library IJL (Intel JPEG Library) as a plugin. When ijl15.dll is present in Plugins directory, the program automatically utilizes it for decoding JPEG files which results in speed increase up to 300%.
* Many optimizations to thumbnail creating mechanism which benefits even more from using IJL and results in just a crazy thumbnail rebuilding speed.
* Andrew Dudko has written initial expression parser routines; this allowed to implement a brand-new picture search interface with using filter expressions.
* The same expression can now be used to filter pictures in a view.
* The former search interface is also available (now called **Simple search**). A possibility of adding an arbitrary number of criteria by any properties of a picture.
* New searchable/displayable picture properties: thumbnail width, height and dimensions.
* Preview feature in the Add pictures wizard at both Select files and Check file pages.
* **Ctrl+Right click** on **View and Adjust** page of the Picture properties dialog now also invokes a shell context menu.
* Other fixes (see the bug tracker for details).

## PhoA 1.1.6 beta
*[Nov 09, 2004]*

* Completely reworked program localization mechanism. All you need to do to add a new interface language is place a translation file into the Language folder. The translation file itself is a plain text one, accessible with any text editor; but you are strongly recommended to use **DKLang Translation Editor** (available on the website) for this purpose since it provides a number of additional, translation-specific features.
* About 80% rewrite of internal program design. This might affect the number of bugs ;)
* Undo data are now stored to the disk, not to memory. This allowed to increase the default undo buffer size to 100 operations.
* Thumbnail window: smooth scrolling; **Ctrl+Arrows/PgUp/PgDn/Home/End** now moves the focus leaving the selection intact; **Shift+Alt+Arrows/PgUp/PgDn/Home/End** creates rectangular selection; **Space/Ctrl+Space** toggles focused thumbnail selection; **Ctrl+Mouse wheel** scrolls one screen per detent.
* Enabled file drag'n'drop into the group tree.
* New resampling filters: **Draft** (more precise than Nearest neighbor but significantly faster than Bilinear/Bicubic) and **Cosine**.
* Additional thumbnail appearance settings: thumbnail background border style and color.
* A possibility of Picture properties dialog default page selection.
* A possibility of picture data editing while a view is active.
* Thumbnail caching in the Browse mode is no longer pertinent. Things showed it caused big problems under Win9x.
* Pressing the **Enter** key in the Browse mode when thumbnail window is active starts viewing of the current picture. Pressing **Shift+Ctrl+Enter** enters the View mode and starts slide show.
* Pressing the **Enter** key in the View mode exits back to the Browse mode provided the last viewed picture is selected in the thumbnails window.
* Tons of minor fixes.

## PhoA 1.1.5 beta
*[Jun 14, 2004]*

* A possibility for nondestructive picture rotation by multiples of 90&deg; and for horizontal and vertical flipping during viewing. Also the transformations may be stored with the picture in the photo album file. Transformation controls on the **View** page (now **View &amp; Adjust**) of Picture properties dialog.
* New picture group properties: ID (an unique numeric identifier), text description, number of pictures and nested groups; the group properties dialog; options for configuring and displaying the group tooltips.
* Improved message boxes: there is the **Don't show this message again** checkbox wherever it is appllicable.
* A possibility for switching the visual theme for toolbars and the status bar.
* Pressing **Ctrl+Enter** in dialogs acts as clicking on **OK**. This allows for applying the changes when caret is in a multiline edit box.
* A possibility for autofilling the picture transformations (rotation and flips) according to metadata.
* File operations wizard: the confirm file overwrite dialog now has also **Yes to all** and **No to all** buttons.
* Splash screen at program startup displaying the current startup status.
* Options for animating the splash screen and **About** dialog fadeout.
* Support for command line keys.
* **View | Remove Search Results** menu item.
* Extended TIFF support.
* Minor changes and fixes.

## PhoA 1.1.4 beta
*[May 16, 2004]*

* File operations wizard: options for selecting pictures with existent or nonexistent files on the picture selection page.
* File masks have been extended: symbol **!** at the start of the mask inverts its meaning.
* Right mouse click when **Ctrl** is pressed invokes shell context menu for the current file for both browse (click on a thumbnail image) and view (click on image) modes.
* View window in **Always on top** mode now doesn't obscure Picture properties and Program settings dialogs.
* Program settings use inplace editing now. Eg., picture properties to display on thumbnails are now being selected from the dropdown list.
* Browse mode options **Thumbnail background color** and **Thumbnail font color**.
* View mode option **Keep mouse cursor over the toolbar button on window resize**.
* View mode option **Show picture info** and **Show info** command in the view mode.
* Add pictures wizard option **Show hidden files and folders**.
* Wizard log pages display record number, total number of records and number of failed entries. Also Copy to clipboard and Find text features implemented.
* Clipboard options allow to specify which formats are placed into the clipboard on **Copy** and **Cut** operations. Possible formats are: PhoA pictures, shell file objects (this allows you to simply copy the necessary files for selected pictures into any folder), plain-text file path list, thumbnail image.
* Implemented invoking the external programs - *tools*: **Tools** page in the settings dialog; a possibility to require additional items in **Tools** and popup menus.
* Fixed invoking the help in dialogs with F1 key.
* Fixed Access Violation when the root group contains pictures and **File | New** menu item is selected.
* Fixed display of file sizes over 2 GB in **Statistics**.
* New images for buttons, menu items etc.
* Reorganized documentation.

## PhoA 1.1.3 beta
*[Mar 29, 2004]*

* New interface language: **Ukrainian** (translation by Serge Mikulin).
* File operations Wizard added allowing to perform the following operation with picture files:
  * **Copying files of selected pictures** into single folder / maintaining original folder layout relatively to specified folder / maintaining picture group hierarchy relatively to specified group (incl. possibility for creating multiple copies of a file when this file is contained in several groups).
  * **Moving picture files of selected pictures** - with the same options as copying; the moving updates file links in corresponding pictures. There is also an option for deleting the original files (wipe or recycle).
  * **Deleting files of selected pictures**, including corresponding pictures.
  * **Rebuilding thumbnails for selected pictures** - this operation is also handled by the Wizard. Moreover, copying and moving also allow to automatically create a separate photo album for the files being copied or moved, to copy the program executable and to create autorun.inf file - the File operations Wizard makes preparing a CD or DVD a snap.
* Possibility for storing and loading settings to/from an ini-file. Also option for autoloading settings from phoa.ini on program startup, if this file is present in the program directory.
* **Picture operations** dialog displays faster.
* Add pictures Wizard now remembers last settings used.
* Toolbar dragging option now affects the View mode toolbar.
* Possibility for sorting the search results.
* Minor fixes.

## PhoA 1.1.2
*[Feb 07, 2004]*

* New interface languages: **German** (by Dominik Mayer) and **Brazilian Portuguese** (by Cesar Boschetti). Now PhoA supports 4 languages!
* Background decoding thread's priority has been lowered. The result is that the picture already decoded is displayed significantly faster.
* A stupid memory leak removed - the background decoding thread didn't terminate when viewing is finished.
* Search by picture dimensions bugs fixed. Searching the file mask now checks just the file's name, not its full path.

## PhoA 1.1.1b
*[Sep 17, 2003]*

* New options for trees/lists: **Incremental search on typing** and **Incremental search cleanup delay**.
* In the **Picture properties dialog** the first control on the page gets focused automatically.
* Changes to **Picture properties | Metadata**: description panel, metatag code is now displayed.
* Changes to **Picture properties | View**: wheel scaling is fixed.
* Changes to **Picture properties | Keywords**: **Checked only** switch; new keywords are painted navy only when not highlighted.
* Changes to **Picture properties | Groups**: speeded up the first time display.
* An attempt made to fix font displaying in both Unicode- and non-Unicode-systems for various codepages/charsets.
* Dialogs have been changed. Fixed large system font issues.
* EXIF-tags list has been expanded, many tags have got descriptions.
* Support for "non-regular" metatags: Unicode (WinXP-tags), Versions etc.
* Fixed issue with PhoA denied to open 0.x version photo albums.
* Fixed view mode bug: predecoding malfunctioned.
* Fixed search bug: date, time and keyword searches were not working properly.
* Fixed some memory leaks.
* Minor changes.

## PhoA 1.1.1a
*[Sep 08, 2003]*

* Photo album file format changed - now it is very expandable and meant not to be changed frequently.
* Modifications to folder tree appearance:
  * **Views** toolbar has been removed. Instead of it there is now a dropdown button beside the photo album node invoking views menu.
  * Pictures are now can be placed directly to the photo album node, having no group created.
  * Number of images is displayed beside the group name.
  * Altered Drag'n'Drop functioning: to insert a node as a sibling of another node (above or below), you'll have to drag the node to the free area on the right side of nodes, and then the insertion point is drawn as a line. To make a node a child of another node you drag this node right over the target node, then the target node gets highlighted. There's no more Shift button used (i mean while dragging ;)
  * When a node is being dragged the target node expands if you leave mouse over the target for a time. Also when mouse rests near the top or bottom edge (while dragging) the group tree starts to scroll.
  * Cosmetic changes (root items are drawn bold, Drag'n'Drop uses blended source folder image etc.)
  * Lists are adjustable.
* Picture adding and editing have been completely separated. The goal was to remove compromises in the interface. Pictures are always being added with the defaults, you may edit them afterwards.
* Changes to the Add pictures dialog:
  * The dialog became the wizard. On the first stage you select picture files and/or folder, on the second one you may exactly determine which files are to be added.
  * When adding pictures you can filter them by photo album presence, by name or extension, by date/time file modified.
  * Picture addition is performed in the separate thread which has improved the response to user actions.
  * Addition process is being logged, the log can be saved to a text file.
* Fully functional support for JPEG-image metadata **EXIF 2.1** - data placed to image files by almost any digicam as well as by modern graphic editors. Metadata can be used for filling some picture properties.
* Many changes to Edit pictures dialog. In a nutshell:
  * Dialog became multipaged.
  * **File properties** page allows you to view file properties info.
  * **Metadata** page displays all the EXIF data contained in JPEG files.
  * **View** page enables file viewing for any selected file.
  * **Data** page is just a thing that was Edit pictures dialog + new properties.
  * **Keywords** page allows you to modify keyword list by adding, removing and editing single keywords for any number of pictures at once.
  * **Groups** page shows photo album group tree in which you can easily determine groups pictures belong to, or change these belongings.
* Program settings were extended aloud.
* You may now reorder pictures inside group using Drag'n'Drop (in the thumbnails pane).
* Views now support automated sorting the pictures inside each group.
* Thumbnail pane always selects the first thumbnail by default.
* Undo history dialog has been removed: now you can undo any number of changes by clicking the dropdown button near the **Undo** toolbar button or with **Edit | Undo history** submenu.
* All other trees displaying picture groups also have been altered - now they all look like the main tree.
* The status bar displays the total number of distinct pictures in the photo album.
* Local toolbar in the view mode.
* Enhanced support for XP themes in toolbars and the thumbnail pane.
* Single installer for both languages.
* Lots of fixes, minor changes and new bugs.

## PhoA 1.0.2a
*[Jun 02, 2003]*

* By a lot of complaints against thumbnail quality there's resampling filtering option to eliminate shrinked picture's fuzziness. Possible filters are: Nearest Neighbor, Linear, B-spline (bicubic), Lanczos, Mitchell. As a side effect, owing to some smoothing of the thumbnail, its size (because of JPEG-compression) reduced by 10 thru 25% (eg, when using B-spline interpolation the decrease is 20-25%).
* Significantly increased speed of rescaling and scrolling pictures. In the view mode you can also choose any of the resampling filters listed above.
* Added option for decoding pictures in background: a separate thread is responsible for decoding the next picture while you are watching current one. This technique minimizes time required to advance to the next picture (but requires more memory).
* Added option for caching-behind viewed pictures, which minimizes time to rewind to the previous picture (but also takes more memory).
* The support for **.gif** files was not included. Fixed.
* Added support for **.msk** (Paint Shop Pro mask), **.tub** (Paint Shop Pro picture tube), **.pfr** (Paint Shop Pro picture frame) file formats.
* View info painting is completely changed. Now information is always positioned over the picture, optionally on a translucent background, and you can visually position information bar.
* In multimonitor systems dialog windows were mispositioned. Fixed.

## PhoA 1.0.1a
*[May 17, 2003]*

* Huge number of differences resulted in program version and time taken to release it.
* **Views!** Completely new picture arranging algorithm. Now you can classify a heap of pictures into folders with minimum efforts. The only requirement is that pictures' properties are properly filled. Possible grouping conditions are: by picture file path, by date (in various ways), by place, by keywords etc. Groupings may be cascaded, ie may be applied one by one, number of groupings is a matter of sanity. The result of the grouping (folder tree along with their pictures) may be easily put into any photo album group, and after then you can tweak them in usual way.
* Photo album file (.phoa) format has been changed. Photo album files since version 1.0.1a contain views' data, and they are incompatible with the earlier **PhoA** versions. The program also supports saving files in PhoA 0.x format, which is choosed while saving the file (File | Save as...); accordingly, in this case views are not saved.
* **Sorting** now can be performed by any number of criteria; it is organized in the similar way as views' groupings. The list of sorting criteria have been significantly extended.
* You can now tweak thumbnail tooltips in Browse mode; you can choose which details are displayed.
* Picture info in View mode is now also more adjustable.
* Three options on **toolbar buttons size** (small/middle/large).
* Massively extended support for **picture file formats**. Now almost all possible formats are supported (briefly: Windows bitmap (BMP, RLE, DIB), OS/2 bitmap, TIFF, GFI fax, SGI, Autodesk, Truevision Targa, ZSoft Paintbrush, Word 5.x, Kodak Photo-CD, Portable pixel/gray map, Dr. Halo CUT, CompuServe GIF, SGI Wavefront, Photoshop, Paint Shop Pro, Portable network graphic PNG).
* New algorithms for **keyword processing** (keywords containing spaces and/or commas are to be quoted; when selecting them from the list such keywords are quoted automatically; keywords are no longer converted to lowercase in the Select keywords dialog, though keywords are still compared case-insensitively).
* A number of minor fixes (including fixed Access Violation in some cases while browsing).

## PhoA 0.10 beta
*[Jan 09, 2003]*

* The fully operational **undo engine** is built into PhoA.
* New button "Add all" in the Add picture dialog, allowing unconditional addition of all picture files selected (using defaults).
* **Clipboard functionality** implemented (cut, copy, paste).
* Minor fixes (Browse mode; photo album stats).

## PhoA 0.03
*[Oct 20, 2002]*

* Completely bilingual version (English &amp; Russian), including Setup program and Help System files.
* Implemented Slide Show mode.
* Minor changes.

## PhoA 0.02b
*[Sep 06, 2002]*

* The first public release.
