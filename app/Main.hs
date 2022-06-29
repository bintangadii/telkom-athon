module Main where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import Module.Item (LogItem (UnknownItem), addNewItem, description, itemId, itemName, parseItem, parseLogItem, restockItem, storage, takeItem)
import Module.Message (LogMessage, makeLogMessage, parseLogMessage)
import System.IO (hFlush, stdout)

runProgram :: [LogItem] -> [LogMessage] -> IO ()
runProgram items messages = do
    putStrLn "\n\n\n=============== Eye on Props ==============="
    putStrLn $ replicate 58 '='
    putStrLn $ showItem items
    putStrLn "(a) Tampilkan seluruh data  (b) Update masa berlaku (c) Hapus asset (d) Tambahkan asset  (e) Keluar"
    choice <- prompt "Pilih action: "
    case choice of
        "a" -> do
            putStrLn $ showAllItem items
            empty <- prompt "Tekan Enter untuk kembali"
            runProgram items messages
        "b" -> do
            putStrLn "Perhatian! Pastikan masa berlaku sudah sesuai: "
            -- Masukkan AssetID
            putStr "Masukkan AssetID: "
            hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            -- Insert Amount
            putStr "Input tambahan masa berlaku: "
            hFlush stdout
            amount <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0

            newRestockedItems <- restockItem items choice amount
            parseLogItem newRestockedItems
            let changedItem = find (\item -> itemId item == choice) newRestockedItems
                extractItem :: Maybe LogItem -> LogItem
                extractItem (Just a) = a
                extractItem Nothing = UnknownItem

            let extractedItem = extractItem changedItem

            logMessage <-
                if extractedItem == UnknownItem
                    then makeLogMessage extractedItem "ERR"
                    else makeLogMessage (extractedItem{storage = amount}) "IN"

            parseLogMessage logMessage
            emptyPrompt <- prompt "Tekan Enter untuk melanjutkan."
            runProgram newRestockedItems messages
        "c" -> do
            putStrLn "Asset akan dihapus, pastikan Asset ID sesuai!"
            -- Masukkan AssetID
            putStr "Masukkan AssetID: "
            hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            -- Insert Amount
            putStr "Insert amount to take: "
            hFlush stdout
            amount <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0

            updatedItems <- takeItem items choice amount
            parseLogItem updatedItems

            let changedItem = find (\item -> itemId item == choice) updatedItems
                extractItem :: Maybe LogItem -> LogItem
                extractItem (Just a) = a
                extractItem Nothing = UnknownItem

            let extractedItem = extractItem changedItem

            logMessage <-
                if extractedItem == UnknownItem
                    then makeLogMessage extractedItem "ERR"
                    else
                        if amount > storage extractedItem
                            then makeLogMessage (extractedItem{storage = 0}) "ERR"
                            else makeLogMessage (extractedItem{storage = amount}) "OUT"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Tekan Enter untuk melanjutkan."
            runProgram updatedItems messages
        "d" -> do
            putStrLn "\nAsset baru akan ditambahkan, Silakan isikan informasi berikut: "
            name <- prompt "No: "
            putStr "Quantity: "
            hFlush stdout
            storage <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            description <- prompt "Keterangan: "
            newItems <- addNewItem items name storage description
            parseLogItem newItems
            logMessage <- makeLogMessage (last newItems) "NEW"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Successfully added new item! Press enter to continue."
            runProgram newItems messages
        "e" -> do
            putStrLn "Exiting program..."
            putStrLn "Goodbye!"
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            runProgram items messages

showItem :: [LogItem] -> String
showItem items = showItemFunc (length items) (take 2 items)
  where
    showItemFunc count [] = case count of
        0 -> "The item list is currently empty.\n" ++ replicate 58 '='
        1 -> "\n" ++ replicate 58 '='
        2 -> "\n" ++ replicate 58 '='
        _ -> "...and " ++ show (count - 2) ++ " more." ++ "\n" ++ replicate 58 '='
    showItemFunc count (item : rest) =
        "ID: " ++ show (itemId item)
            ++ "\nSTO: "
            ++ itemName item
            ++ "\nNo Sertifikat: "
            ++ show (storage item)
            ++ "\nJenis Sertifikat: "
            ++ description item
            ++ "\nLuas Tanah: "
            ++ tanggal terbit '-'
            ++ "\nTanggal Terbit: "
            ++ tanggal berakhir
            ++ "\nTanggal Berakhir: "
            ++ jangka waktu
            ++ "\nJangka Waktu: "
            ++ keterangan
            ++ "n\Keterangan: "
            ++ showItemFunc count rest

showAllItem :: [LogItem] -> String
showAllItem [] = replicate 58 '='
showAllItem (item : rest) =
    "ID: " ++ show (itemId item)
        ++ "\nSTO: "
        ++ itemName item
        ++ "\nNo Sertifikat: "
        ++ show (storage item)
        ++ "\nJenis Sertifikat: "
        ++ description item
        ++ "\nLuas Tanah: "
        ++ tanggal terbit '-'
        ++ "\nTanggal Terbit: "
        ++ tanggal berakhir
        ++ "\nTanggal Berakhir: "
        ++ jangka waktu
        ++ "\nJangka Waktu: "
        ++ keterangan
        ++ "n\Keterangan: "
        ++ showAllItem rest

main :: IO ()
main = do
    items <- fmap parseItem (readFile "log/items.log")
    runProgram items []