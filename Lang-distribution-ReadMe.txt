$Id: Lang-distribution-ReadMe.txt,v 1.3 2004-05-20 11:50:54 dale Exp $
------------------------------------------------------------------------------------------------------------------------
Dmitry Kann :: http://phoa.narod.ru
Notes on language files distribution
Applicable to PhoA version 1.1.5 beta
------------------------------------------------------------------------------------------------------------------------

For RUSSIAN: См. далее по тексту

The distribution includes the following files:

  - dttm\dttm.exe                - The DaleTech Translation Manager 
  - *.xdtls                      - DaleTech XML language snapshots
  - IS-install\phoa.iss          - PhoA setup script
  - IS-install\eula-eng.rtf      - Original English End-User License Agreement
  - IS-install\eula-rus.rtf      - Original Russian End-User License Agreement
  - Lang-distribution-ReadMe.txt - This file

Localizing the main program files
------------------------------------------------------------------------------------------------------------------------

* Note for translators of previous PhoA versions:
*   Language files since PhoA 1.1.4 beta are of XML format and have different
*   extension (.xdtls instead of .dtls). This allowed to achieve correct
*   version controlling and diffing using CVS (while it has very poor support
*   for versioning binary files). Don't try to open or edit those files with 
*   previous versions of Translation Manager as it is unable to open XML
*   language files. Also, save the translated files only as .xdtls files.

Generally, you use the Translation Manager (dttm.exe) to open the accompanying
*.xdtls files.

Then you must add the language you want to translate to by selecting the
Edit | Add language... menu item. I highly recommend highlighting the English
language column before, then adding your language having set 'Copy all current
language data to the new language' check box on.

The original program languages are Russian and English. So if you see that
translations of a property for these languages are the same, there's no need
(and even may be wrong) to localize this property.

Properties you should never translate:
  SecondaryShortCuts
  HelpKeyword
  all Ini- or registry-related entries (IniFileName, IniSection)
  mruOpen.Prefix
  all URL-related entries
  Application name (PhoA)

WARNING: the Translation Manager software was written by me, and the only
tester was me, so never expect it to be bug-free or even stable! Make backups
of all your work as often as you change the files. Well, you're warned ;)

Localizing the setup script
------------------------------------------------------------------------------------------------------------------------

You open the supplied SetupMessages.txt file in any text editor capable of editing
plain-text files in your language.

Then you should find the section for the language you translate to. The file is
pretty self-explanatory, but if it isn't clear yet, feel free to ask me.

Localizing the End-User License Agreement
------------------------------------------------------------------------------------------------------------------------

Use Microsoft Word to translate the License Agreement into your language. Then
save it as *.rtf format.

How do I check the results?
------------------------------------------------------------------------------------------------------------------------

You cannot view the final results of your translation before I've recompiled
the program. So send me all you've done, then I build the application and
send it back to you - for you to be able to check the accuracy of the
translation.

*******************************************************************************
Please note: I do NOT encourage any secondary redistribution of any of these
files! All the language files as well as the Translation Manager software are
my sole property, and I allow using them for personal use only!
*******************************************************************************

Personally, I want to ask you to be as accurate as you can, please. Just
because I always do the same ;)

Sure, I will mention the work you have done in every relevant place!

Thanks and good luck,
Dmitry Kann, phoa@narod.ru


=== The same in Russian ================================================================================================

Пакет включает в себя следующие файлы:

  - dttm\dttm.exe                - The DaleTech Translation Manager 
  - *.xdtls                      - Языковые файлы DaleTech формата XML
  - IS-install\phoa.iss          - Установочный сценарий PhoA
  - IS-install\eula-eng.rtf      - Оригинальное Лицензионное Соглашение на английском языке
  - IS-install\eula-rus.rtf      - Оригинальное Лицензионное Соглашение на русском языке
  - Lang-distribution-ReadMe.txt - Этот файл

Локализация основных файлов программы
------------------------------------------------------------------------------------------------------------------------

* Примечание для переводчиков предыдущих версий PhoA:
*   Языковые файлы, начиная с версии PhoA 1.1.4 beta, являются файлами формата XML
*   и имеют другое расширение (.xdtls вместо .dtls). Это позволило реализовать
*   надлежащий контроль версий в CVS (для бинарных файлов поддержка там очень
*   ограниченна). Не пытайтесь открывать или редактировать эти файлы старыми версиями
*   Translation Manager, поскольку он не может открывать языковые файлы формата XML.
*   Кроме того, сохраняйте переведённые файлы только как файлы .xdtls.

Если кратко, вы должны использовать Translation Manager (dttm.exe), чтобы открывать и
редактировать прилагаемые файлы *.xdtls.

Потом вы должны добавить язык, на который вы выполняете перевод, выбрав пункт меню
Edit | Add language... Я очень рекомендую выделить сначала столбец с языком, который
послужит отправным для перевода (английский или русский), а потом установить флажок
в переключателе 'Copy all current language data to the new language', чтобы
скопировать из него все значения в новый язык.

Оригинальными языками программы являются русский и английский. Поэтому если вы видите, что
перевод какого-либо свойства в этих языках совпадает, не нужно (и даже, возможно, неверно)
переводить это свойство.

Свойства, которые вы никогда не должны переводить:
  SecondaryShortCuts
  HelpKeyword
  все Ini-записи и записи, относящиеся к реестру (IniFileName, IniSection)
  mruOpen.Prefix
  все записи, относящиеся к URL
  Наименование приложения (PhoA)

ПРЕДУПРЕЖДЕНИЕ: программа Translation Manager написана мной, и единственным, кто её
тестировал, был я, поэтому никогда не полагайтесь на то, что она работает без сбоев, и даже
на её стабильность! Делайте резервные копии всей вашей работы после каждого изменения
файлов. В общем, я вас предупредил ;)

Локализация сценария установки
------------------------------------------------------------------------------------------------------------------------

Вы должны открыть прилагаемый файл SetupMessages.txt любым текстовым редактором, способным
редактировать неформатированные текстовые файлы на вашем языке.

Потом вам нужно найти раздел, соответствующий языку, на который вы переводите. Формат файла
сам по себе достаточно понятен, но при любых неясностях вы можете обратиться ко мне.

Локализация Лицензионного Соглашения
------------------------------------------------------------------------------------------------------------------------

Используйте Microsoft Word, чтобы перевести Лицензионное Соглашение на ваш язык. Потом сохраните
его в формате *.rtf.

Как мне проверить результат перевода?
------------------------------------------------------------------------------------------------------------------------

Вы не можете просмотреть окончательный результат перевода до тех пор, пока я не перекомпилирую
приложение. Поэтому отправляйте мне всё, что вы сделали, я соберу программу и пришлю вам опять -
чтобы вы могли проверить точность перевода.

*******************************************************************************
Пожалуйста, запомните: я НЕ одобряю любое вторичное распространение любых
файлов, включённых в данный пакет! Все языковые файлы, а также программа
Translation Manager являются исключительно моей собственностью, и я даю
разрешение только для их персонального использования!
*******************************************************************************

Лично я хотел бы попросить вас быть настолько аккуратными (в переводе), насколько это только
возможно. Просто потому, что я сам всегда поступаю именно так ;)

Разумеется, ваша работа будет упомянута в каждом подходящем месте документации!

Спасибо и удачи,
Дмитрий Канн, phoa@narod.ru
