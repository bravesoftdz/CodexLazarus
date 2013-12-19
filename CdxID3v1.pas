unit CdxID3v1;
{
------------------------------------------------------------------------
Unit Information:
------------------------------------------------------------------------
Name:       CdxID3v1
Version:    1.1
Purpose:    Read ID3 V1 Tags from MP3 music files
Copyright:  Alexander Feuster
Contact:    alexander.feuster@gmail.com
URL:        https://github.com/feuster/CodexLazarus
Licence:    GPLv2
            http://www.gnu.org/licenses/gpl-2.0

------------------------------------------------------------------------
Version History:
------------------------------------------------------------------------
1.0   23.07.2009    Initial version written in D7
1.1   19.12.2013    Rework as FPC/Lazarus version

}
interface

uses
  SysUtils, Classes;

  //Tag type
  type ID3v1Tag=packed record
        SongTitle:  String;
        Artist:     String;
        Album:      String;
        Year:       String;
        Comment:    String;
        AlbumTrack: String;
        Genre:      String;
      end;
      
  //Tag array type
  type ID3v1TagArray=array[0..127] of Byte;

  //Genre const array according to ID3v1 genre specification including Winamp extension
  const ID3v1GenresArray: array[0..125] of String = (
    'Blues', 'Classic Rock', 'Country', 'Dance', 'Disco', 'Funk', 'Grunge', 'Hip-Hop',
    'Jazz', 'Metal', 'New Age', 'Oldies', 'Other', 'Pop', 'Rhythm and Blues', 'Rap',
    'Reggae', 'Rock', 'Techno', 'Industrial', 'Alternative', 'Ska', 'Death Metal', 'Pranks',
    'Soundtrack', 'Euro-Techno', 'Ambient', 'Trip-Hop', 'Vocal', 'Jazz&Funk', 'Fusion', 'Trance',
    'Classical', 'Instrumental', 'Acid', 'House', 'Game', 'Sound Clip', 'Gospel', 'Noise',
    'Alternative Rock', 'Bass', 'Soul', 'Punk', 'Space', 'Meditative', 'Instrumental Pop', 'Instrumental Rock',
    'Ethnic', 'Gothic', 'Darkwave', 'Techno-Industrial', 'Electronic', 'Pop-Folk', 'Eurodance', 'Dream',
    'Southern Rock', 'Comedy', 'Cult', 'Gangsta', 'Top 40', 'Christian Rap', 'Pop/Funk', 'Jungle',
    'Native US', 'Cabaret', 'New Wave', 'Psychedelic', 'Rave', 'Showtunes', 'Trailer', 'Lo-Fi',
    'Tribal', 'Acid Punk', 'Acid Jazz', 'Polka', 'Retro', 'Musical', 'Rock & Roll', 'Hard Rock',
    'Folk', 'Folk-Rock', 'National Folk', 'Swing', 'Fast Fusion', 'Bebop', 'Latin', 'Revival',
    'Celtic', 'Bluegrass', 'Avantgarde', 'Gothic Rock', 'Progressive Rock', 'Psychedelic Rock', 'Symphonic Rock', 'Slow Rock',
    'Big Band', 'Chorus', 'Easy Listening', 'Acoustic', 'Humour', 'Speech', 'Chanson', 'Opera',
    'Chamber Music', 'Sonata', 'Symphony', 'Booty Bass', 'Primus', 'Porn Groove', 'Satire', 'Slow Jam',
    'Club', 'Tango', 'Samba', 'Folklore', 'Ballad', 'Power Ballad', 'Rhythmic Soul', 'Freestyle',
    'Duet', 'Punk Rock', 'Drum Solo', 'Acapella', 'Euro-House', 'Dance Hall'
    );

  procedure ID3v1ClearTagArray(var Tag: ID3v1TagArray);
  procedure ID3v1ClearTag(var Tag: ID3v1Tag);
  function ID3v1ReadTagFromFile(FileName: String; var Tag: ID3v1Tag): Boolean;
  function ID3v1ReadTagArrayFromFile(FileName: String; var Tag: ID3v1TagArray): Boolean;
  function ID3v1ReadTitleFromTagArray(Tag: ID3v1TagArray): String;
  function ID3v1ReadArtistFromTagArray(Tag: ID3v1TagArray): String;
  function ID3v1ReadAlbumFromTagArray(Tag: ID3v1TagArray): String;
  function ID3v1ReadYearFromTagArray(Tag: ID3v1TagArray): String;
  function ID3v1ReadCommentFromTagArray(Tag: ID3v1TagArray): String;
  function ID3v1ReadTrackFromTagArray(Tag: ID3v1TagArray): String;
  function ID3v1ReadGenreFromTagArray(Tag: ID3v1TagArray): String;
  function ID3v1GenreString(Genre: Byte): String;
  function ID3v1GenreByte(Genre: String): Byte;

implementation

procedure ID3v1ClearTag(var Tag: ID3v1Tag);
//clear ID3 tag
begin
  Tag.SongTitle:='';
  Tag.Artist:='';
  Tag.Album:='';
  Tag.Year:='';
  Tag.Comment:='';
  Tag.AlbumTrack:='';
  Tag.Genre:='';
end;

procedure ID3v1ClearTagArray(var Tag: ID3v1TagArray);
//clear ID3 tag array
var
  Index: Byte;

begin
  for Index:=0 to 127 do
    Tag[Index]:=0;
end;

function ID3v1ReadTagFromFile(FileName: String; var Tag: ID3v1Tag): Boolean;
//read ID3v1 tag into special tag type
var
  TagArray: ID3v1TagArray;

begin
  //read tag array from file
  if ID3v1ReadTagArrayFromFile(FileName,TagArray)=false then
    begin
      result:=false;
      exit;
    end;

  //read ID3v1 infos from tag array
  try
  ID3v1ClearTag(Tag);
  Tag.SongTitle:=ID3v1ReadTitleFromTagArray(TagArray);
  Tag.Artist:=ID3v1ReadArtistFromTagArray(TagArray);
  Tag.Album:=ID3v1ReadAlbumFromTagArray(TagArray);
  Tag.Year:=ID3v1ReadYearFromTagArray(TagArray);
  Tag.Comment:=ID3v1ReadCommentFromTagArray(TagArray);
  Tag.AlbumTrack:=ID3v1ReadTrackFromTagArray(TagArray);
  Tag.Genre:=ID3v1ReadGenreFromTagArray(TagArray);
  result:=true;
  except;
  ID3v1ClearTag(Tag);
  result:=false;
  end;
end;

function ID3v1ReadTagArrayFromFile(FileName: String; var Tag: ID3v1TagArray): Boolean;
//read ID3v1 tag into 128 byte array
var
  MP3Stream: TFileStream;

begin
  //check if MP3 exists
  if FileExists(FileName)=false then
    begin
      result:=false;
      exit;
    end;

  try
    //clear tag buffer
    ID3v1ClearTagArray(Tag);

    //read tag from MP3 file which has a minimum of 128 bytes
    MP3Stream:=TFileStream.Create(Filename, fmOpenRead);
    if MP3Stream.Size>128 then
      begin
        MP3Stream.Seek(MP3Stream.Size-128,0);
        MP3Stream.ReadBuffer(Tag, 128);
      end;

    //check if tag exists
    if (Tag[0]=84) and (Tag[1]=65) and (Tag[2]=71) then
      result:=true
    else
      begin
        ID3v1ClearTagArray(Tag);
        result:=false;
      end;

    //clear temp file stream
    if MP3Stream<>nil then
      MP3Stream.Free;
  except
    result:=false;
  end;
end;

function ID3v1ReadTitleFromTagArray(Tag: ID3v1TagArray): String;
//read ID3v1 title info from tag
var
  Buffer: String;
  Index: Byte;

begin
  //read title info from tag
  Buffer:='';
  for Index:=3 to 33 do
    begin
      if Tag[Index]>0 then
        Buffer:=Buffer+Chr(Tag[Index])
      else
        break;
    end;

  //result
  Trim(Buffer);
  if Buffer<>'' then
    result:=Buffer
  else
    result:='';
end;

function ID3v1ReadArtistFromTagArray(Tag: ID3v1TagArray): String;
//read ID3v1 artist info from tag
var
  Buffer: String;
  Index: Byte;

begin
  //read artist info from tag
  Buffer:='';
  for Index:=33 to 62 do
    begin
      if Tag[Index]>0 then
        Buffer:=Buffer+Chr(Tag[Index])
      else
        break;
    end;

  //result
  Trim(Buffer);
  if Buffer<>'' then
    result:=Buffer
  else
    result:='';
end;

function ID3v1ReadAlbumFromTagArray(Tag: ID3v1TagArray): String;
//read ID3v1 album info from tag
var
  Buffer: String;
  Index: Byte;

begin
  //read album info from tag
  Buffer:='';
  for Index:=63 to 92 do
    begin
      if Tag[Index]>0 then
        Buffer:=Buffer+Chr(Tag[Index])
      else
        break;
    end;

  //result
  Trim(Buffer);
  if Buffer<>'' then
    result:=Buffer
  else
    result:='';
end;

function ID3v1ReadYearFromTagArray(Tag: ID3v1TagArray): String;
//read ID3v1 year info from tag
var
  Buffer: String;
  Index: Byte;

begin
  //read year info from tag
  Buffer:='';
  for Index:=93 to 96 do
    begin
      if Tag[Index]>0 then
        Buffer:=Buffer+Chr(Tag[Index])
      else
        break;
    end;

  //result
  Trim(Buffer);
  if Buffer<>'' then
    result:=Buffer
  else
    result:='';
end;

function ID3v1ReadCommentFromTagArray(Tag: ID3v1TagArray): String;
//read ID3v1 comment info from tag
var
  Buffer: String;
  Index: Byte;

begin
  //read comment info from tag
  Buffer:='';
  for Index:=97 to 126 do
    begin
      if Tag[Index]>0 then
        Buffer:=Buffer+Chr(Tag[Index])
      else
        break;
    end;

  //result
  Trim(Buffer);
  if Buffer<>'' then
    result:=Buffer
  else
    result:='';
end;

function ID3v1ReadTrackFromTagArray(Tag: ID3v1TagArray): String;
//read ID3v1 album track info from tag
begin
  //read album track info from tag
  if (Tag[125]=0) and (Tag[126]>0) then
    result:=IntToStr(Tag[126])
  else
    result:='';
end;

function ID3v1ReadGenreFromTagArray(Tag: ID3v1TagArray): String;
//read ID3v1 genre info from tag
begin
  //read genre info from tag
    result:=ID3v1GenreString(Tag[127]);
end;

function ID3v1GenreString(Genre: Byte): String;
//Get genre name from ID byte
begin
  if Genre<Length(ID3v1GenresArray) then
    begin
      Result:=ID3v1GenresArray[Genre];
    end
  else
    Result:='';
end;

function ID3v1GenreByte(Genre: String): Byte;
//Retrieve genre ID from name
var
  Index: Byte;

begin
  Index:=0;
  Result:=12; //Genre 'Other' as fallback default
  if Genre<>'' then
    begin
      while Index<Length(ID3v1GenresArray) do
        begin
          if UpperCase(Genre)=UpperCase(ID3v1GenresArray[Index]) then
            begin
              Result:=Index;
              break;
            end;
          inc(Index);
        end;
    end;
end;

end.

